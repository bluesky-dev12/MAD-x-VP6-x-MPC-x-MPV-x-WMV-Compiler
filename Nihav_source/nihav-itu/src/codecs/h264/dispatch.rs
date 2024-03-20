use std::sync::{Arc, Barrier};
use std::sync::atomic::*;
use std::thread;

use nihav_core::codecs::{DecoderError, DecoderResult};

use super::{FrameDecoder, PictureInfo, Shareable};

#[derive(Clone,Copy,Debug,PartialEq)]
pub enum FrameDecodingStatus {
    Ok,
    NotReady,
    Error,
    NotFound,
}

struct FrameState {
    pinfo:      PictureInfo,
    mb_pos:     AtomicUsize,
    error:      AtomicBool,
    complete:   AtomicBool,
    output:     AtomicBool,
    worker:     Option<thread::JoinHandle<DecoderResult<()>>>,
    result:     DecoderResult<()>,
    num_refs:   usize,
    ref_frames: Vec<u32>,
}

impl FrameState {
    fn get_id(&self) -> u32 { self.pinfo.full_id }
    fn get_user_id(&self) -> u32 { self.pinfo.user_id }
    fn is_working(&self) -> bool {
        self.worker.is_some() &&
        !self.complete.load(Ordering::Relaxed) &&
        !self.error.load(Ordering::Relaxed)
    }
    fn is_output_candidate(&self) -> bool {
        !self.output.load(Ordering::Relaxed) &&
            (self.complete.load(Ordering::Relaxed) || self.error.load(Ordering::Relaxed))
    }
}

pub struct ThreadDispatcher {
        fstate:         Vec<FrameState>,
    pub max_threads:    usize,
        cur_threads:    usize,
}

impl ThreadDispatcher {
    pub fn new() -> Self {
        Self {
            fstate:         Vec::new(),
            max_threads:    3,
            cur_threads:    0,
        }
    }
    pub fn can_decode_more(&self) -> bool {
        let out_cand = self.fstate.iter().filter(|state| state.is_output_candidate()).count();
        if out_cand > self.max_threads {
            return false;
        }
        if (self.cur_threads < self.max_threads) || (self.max_threads == 0) {
            true
        } else {
            let real_workers = self.fstate.iter().fold(0usize,
                    |acc, state| acc + (state.is_working() as usize));
            real_workers < self.max_threads
        }
    }
    fn cleanup(&mut self) {
        for state in self.fstate.iter_mut() {
            if state.worker.is_some() && !state.is_working() {
                let mut ret = None;
                std::mem::swap(&mut state.worker, &mut ret);
                if let Some(handle) = ret {
                    state.result = handle.join().unwrap();
                }
                self.cur_threads -= 1;
            }
        }
    }
    fn unref_frame(&mut self, id: u32) {
        let mut toremove = Vec::new();
        for state in self.fstate.iter() {
            if state.num_refs == 0 && state.output.load(Ordering::Relaxed) {
                toremove.push(state.get_id());
            }
        }
        if let Some(idx) = self.find_by_id(id) {
            let mut ref_frm = Vec::new();
            std::mem::swap(&mut ref_frm, &mut self.fstate[idx].ref_frames);
            for state in self.fstate.iter_mut() {
                if ref_frm.contains(&state.get_id()) {
                    assert!(state.num_refs >= 2);
                    state.num_refs -= 2;
                }
            }
            if self.fstate[idx].num_refs == 0 && self.fstate[idx].output.load(Ordering::Relaxed) {
                self.remove_frame(id);
            }
        }
        for &id in toremove.iter() {
            self.remove_frame(id);
        }
    }
    fn find_by_id(&self, id: u32) -> Option<usize> {
        self.fstate.iter().position(|x| x.get_id() == id)
    }
    fn set_completed(&self, id: u32) {
        if let Some(idx) = self.find_by_id(id) {
            self.fstate[idx].complete.store(true, Ordering::Relaxed);
        }
    }
    fn set_error(&self, id: u32) {
        if let Some(idx) = self.find_by_id(id) {
            self.fstate[idx].error.store(true, Ordering::Relaxed);
        }
    }
    pub fn update_pos(&self, id: u32, mb_pos: usize) {
        if let Some(idx) = self.find_by_id(id) {
            self.fstate[idx].mb_pos.store(mb_pos, Ordering::Relaxed);
        }
    }
    pub fn check_pos(&self, id: u32, mb_pos: usize) -> FrameDecodingStatus {
        if let Some(idx) = self.find_by_id(id) {
            let state = &self.fstate[idx];
            if !state.error.load(Ordering::Relaxed) {
                if state.complete.load(Ordering::Relaxed) || mb_pos < state.mb_pos.load(Ordering::Relaxed) {
                    FrameDecodingStatus::Ok
                } else {
                    FrameDecodingStatus::NotReady
                }
            } else {
                FrameDecodingStatus::Error
            }
        } else {
            FrameDecodingStatus::NotFound
        }
    }
    fn remove_frame(&mut self, id: u32) {
        if let Some(idx) = self.find_by_id(id) {
            self.fstate.remove(idx);
        }
    }
    /*fn print_state(&self) {
        print!("   state:");
        for state in self.fstate.iter() {
            print!(" s{}b{}r{}{}{}{}", state.get_id(),
                state.mb_pos.load(Ordering::Relaxed), state.num_refs,
                if state.error.load(Ordering::Relaxed) { "E" } else {""},
                if state.complete.load(Ordering::Relaxed) {"C"} else {""},
                if state.output.load(Ordering::Relaxed) {"O"} else {""});
        }
        println!();
    }*/
    pub fn has_output(&self) -> bool {
        for state in self.fstate.iter() {
            if state.is_output_candidate() {
                return true;
            }
        }
        false
    }
}

pub fn queue_decoding(disp: &mut Shareable<ThreadDispatcher>, mut fdec: FrameDecoder, initial_ref_frames: &[u32], ref_frames: &[u32]) {
    let barrier = Arc::new(Barrier::new(2));
    let starter = Arc::clone(&barrier);

    let pinfo = fdec.cur_pic.clone();
    let pic_id = pinfo.full_id;
    let shared_disp = Arc::clone(disp);
    let worker = thread::Builder::new().name("frame ".to_string() + &pic_id.to_string()).spawn(move || {
            barrier.wait();

            let mut slices = Vec::new();
            std::mem::swap(&mut slices, &mut fdec.slices);
            let mut cur_mb = 0;
            for (hdr, hdr_size, refs, nal) in slices.iter() {
                if hdr.first_mb_in_slice != cur_mb {
                    if let Ok(rd) = shared_disp.read() {
                        rd.set_error(pic_id);
                    } else {
                        panic!("can't set error");
                    }
                    return Err(DecoderError::InvalidData);
                }
                match fdec.decode_slice(hdr, *hdr_size, refs, nal) {
                    Ok(pos) => cur_mb = pos,
                    Err(err) => {
                        if let Ok(rd) = shared_disp.read() {
                            rd.set_error(pic_id);
                        } else {
                            panic!("can't set error");
                        }
                        return Err(err);
                    },
                };
            }

            if cur_mb == fdec.num_mbs {
                if let Ok(rd) = shared_disp.read() {
                    rd.set_completed(pic_id);
                } else {
                    panic!("can't set status");
                }
            }

            DecoderResult::Ok(())
        }).unwrap();
    let new_state = FrameState {
        pinfo,
        mb_pos:     AtomicUsize::new(0),
        error:      AtomicBool::new(false),
        complete:   AtomicBool::new(false),
        output:     AtomicBool::new(false),
        worker:     Some(worker),
        result:     DecoderResult::Err(DecoderError::Bug),
        num_refs:   0,
        ref_frames: initial_ref_frames.to_vec(),
    };
    if let Ok(ref mut ds) = disp.write() {
        let new_id = new_state.get_id();
        if ds.find_by_id(new_id).is_some() {
            ds.remove_frame(new_id);
        }
        ds.cleanup();
        ds.fstate.push(new_state);
        for state in ds.fstate.iter_mut() {
            if ref_frames.contains(&state.get_id()) {
                state.num_refs += 1;
            }
            if initial_ref_frames.contains(&state.get_id()) {
                state.num_refs += 1;
            }
        }
        ds.cur_threads += 1;
        starter.wait();
    } else {
        panic!("cannot invoke thread dispatcher");
    }
}

pub fn wait_for_one(dispatch: &mut Shareable<ThreadDispatcher>) -> Result<PictureInfo, (DecoderError, u32)> {
    /*if let Ok(ref ds) = dispatch.read() {
        ds.print_state();
    }*/
    let start = std::time::Instant::now();
    'main_loop: loop {
        if std::time::Instant::now().duration_since(start) > std::time::Duration::from_millis(20000) { panic!(" too long!"); }
        if let Ok(ref ds) = dispatch.read() {
            let mut nw = 0;
            for state in ds.fstate.iter() {
                if state.is_working() {
                    nw += 1;
                }
                if state.is_output_candidate() {
                    break 'main_loop;
                }
            }
            if nw == 0 {
                return Err((DecoderError::NoFrame, 0));
            }
        } else {
            panic!("can't peek into status");
        }
        thread::yield_now();
    }
    if let Ok(ref mut ds) = dispatch.write() {
        ds.cleanup();
        let mut found = None;
        for state in ds.fstate.iter() {
            if state.is_output_candidate() {
                state.output.store(true, Ordering::Relaxed);
                if let DecoderResult::Err(err) = state.result {
                    let id = state.get_id();
                    let user_id = state.get_user_id();
                    ds.unref_frame(id);
                    return Err((err, user_id));
                } else {
                    found = Some(state.pinfo.clone());
                    break;
                }
            }
        }
        if let Some(ret) = found {
            ds.unref_frame(ret.full_id);
            Ok(ret)
        } else {
            unreachable!();
        }
    } else {
        panic!("can't grab status");
    }
}

pub fn clear_threads(dispatch: &mut Shareable<ThreadDispatcher>) {
    /*if let Ok(ref ds) = dispatch.read() {
        ds.print_state();
    }*/
    let mut to_wait = Vec::new();
    if let Ok(ref mut ds) = dispatch.write() {
        while let Some(state) = ds.fstate.pop() {
            if let Some(handle) = state.worker {
                to_wait.push(handle);
            }
        }
        ds.cur_threads = 0;
    } else {
        panic!("can't grab status");
    }
    while let Some(handle) = to_wait.pop() {
        let _ = handle.join();
    }
}
