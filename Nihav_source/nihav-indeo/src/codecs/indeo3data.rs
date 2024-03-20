pub struct IviDeltaCB {
    pub quad_radix: u8,
    pub data: &'static [i8],
}

const DT_1_1: IviDeltaCB = IviDeltaCB{ quad_radix: 7, data: &[
       0,    0,    2,    2,   -2,   -2,   -1,    3,
       1,   -3,    3,   -1,   -3,    1,    4,    4,
      -4,   -4,    1,    5,   -1,   -5,    5,    1,
      -5,   -1,   -4,    4,    4,   -4,   -2,    6,
       2,   -6,    6,   -2,   -6,    2,    4,    9,
      -4,   -9,    9,    4,   -9,   -4,    9,    9,
      -9,   -9,    1,   10,   -1,  -10,   10,    1,
     -10,   -1,   -5,    8,    5,   -8,    8,   -5,
      -8,    5,    9,   15,   -9,  -15,   15,    9,
     -15,   -9,   -3,   12,    3,  -12,   12,   -3,
     -12,    3,    4,   16,   -4,  -16,   16,    4,
     -16,   -4,   16,   16,  -16,  -16,    0,   18,
       0,  -18,   18,    0,  -18,    0,  -12,   12,
      12,  -12,   -9,   16,    9,  -16,   16,   -9,
     -16,    9,   11,   27,  -11,  -27,   27,   11,
     -27,  -11,   19,   28,  -19,  -28,   28,   19,
     -28,  -19,   -6,   22,    6,  -22,   22,   -6,
     -22,    6,    4,   29,   -4,  -29,   29,    4,
     -29,   -4,   30,   30,  -30,  -30,   -2,   33,
       2,  -33,   33,   -2,  -33,    2,  -18,   23,
      18,  -23,   23,  -18,  -23,   18,  -15,   30,
      15,  -30,   30,  -15,  -30,   15,   22,   46,
     -22,  -46,   46,   22,  -46,  -22,   13,   47,
     -13,  -47,   47,   13,  -47,  -13,   35,   49,
     -35,  -49,   49,   35,  -49,  -35,  -11,   41,
      11,  -41,   41,  -11,  -41,   11,    4,   51,
      -4,  -51,   51,    4,  -51,   -4,   54,   54,
     -54,  -54,  -34,   34,   34,  -34,  -29,   42,
      29,  -42,   42,  -29,  -42,   29,   -6,   60,
       6,  -60,   60,   -6,  -60,    6,   27,   76,
     -27,  -76,   76,   27,  -76,  -27,   43,   77,
     -43,  -77,   77,   43,  -77,  -43,  -24,   55,
      24,  -55,   55,  -24,  -55,   24,   14,   79,
     -14,  -79,   79,   14,  -79,  -14,   63,   83,
     -63,  -83,   83,   63,  -83,  -63,  -20,   74,
      20,  -74,   74,  -20,  -74,   20,    2,   88,
      -2,  -88,   88,    2,  -88,   -2,   93,   93,
     -93,  -93,  -52,   61,   52,  -61,   61,  -52,
     -61,   52,   52,  120,  -52, -120,  120,   52,
    -120,  -52,  -45,   75,   45,  -75,   75,  -45,
     -75,   45,   75,  125,  -75, -125,  125,   75,
    -125,  -75,   33,  122,  -33, -122,  122,   33,
    -122,  -33,  -13,  103,   13, -103,  103,  -13,
    -103,   13,  -40,   96,   40,  -96,   96,  -40,
     -96,   40,  -34,  127,   34, -127,  127,  -34,
    -127,   34,  -89,   89,   89,  -89,  -78,  105,
      78, -105,  105,  -78, -105,   78,   12,   12,
     -12,  -12,   23,   23,  -23,  -23,   42,   42,
     -42,  -42,   73,   73,  -73,  -73,
]};

const DT_1_2: IviDeltaCB = IviDeltaCB{ quad_radix: 9, data: &[
       0,    0,    3,    3,   -3,   -3,   -1,    4,
       1,   -4,    4,   -1,   -4,    1,    7,    7,
      -7,   -7,    2,    8,   -2,   -8,    8,    2,
      -8,   -2,   -2,    9,    2,   -9,    9,   -2,
      -9,    2,   -6,    6,    6,   -6,    6,   13,
      -6,  -13,   13,    6,  -13,   -6,   13,   13,
     -13,  -13,    1,   14,   -1,  -14,   14,    1,
     -14,   -1,   -8,   12,    8,  -12,   12,   -8,
     -12,    8,   14,   23,  -14,  -23,   23,   14,
     -23,  -14,   -5,   18,    5,  -18,   18,   -5,
     -18,    5,    6,   24,   -6,  -24,   24,    6,
     -24,   -6,   24,   24,  -24,  -24,   -1,   27,
       1,  -27,   27,   -1,  -27,    1,  -17,   17,
      17,  -17,  -13,   23,   13,  -23,   23,  -13,
     -23,   13,   16,   40,  -16,  -40,   40,   16,
     -40,  -16,   28,   41,  -28,  -41,   41,   28,
     -41,  -28,   -9,   33,    9,  -33,   33,   -9,
     -33,    9,    6,   43,   -6,  -43,   43,    6,
     -43,   -6,   46,   46,  -46,  -46,   -4,   50,
       4,  -50,   50,   -4,  -50,    4,  -27,   34,
      27,  -34,   34,  -27,  -34,   27,  -22,   45,
      22,  -45,   45,  -22,  -45,   22,   34,   69,
     -34,  -69,   69,   34,  -69,  -34,   19,   70,
     -19,  -70,   70,   19,  -70,  -19,   53,   73,
     -53,  -73,   73,   53,  -73,  -53,  -17,   62,
      17,  -62,   62,  -17,  -62,   17,    5,   77,
      -5,  -77,   77,    5,  -77,   -5,   82,   82,
     -82,  -82,  -51,   51,   51,  -51,  -43,   64,
      43,  -64,   64,  -43,  -64,   43,  -10,   90,
      10,  -90,   90,  -10,  -90,   10,   41,  114,
     -41, -114,  114,   41, -114,  -41,   64,  116,
     -64, -116,  116,   64, -116,  -64,  -37,   82,
      37,  -82,   82,  -37,  -82,   37,   22,  119,
     -22, -119,  119,   22, -119,  -22,   95,  124,
     -95, -124,  124,   95, -124,  -95,  -30,  111,
      30, -111,  111,  -30, -111,   30,  -78,   92,
      78,  -92,   92,  -78,  -92,   78,  -68,  113,
      68, -113,  113,  -68, -113,   68,   18,   18,
     -18,  -18,   34,   34,  -34,  -34,   63,   63,
     -63,  -63,  109,  109, -109, -109,
]};

const DT_1_3: IviDeltaCB = IviDeltaCB{ quad_radix: 10, data: &[
       0,    0,    4,    4,   -4,   -4,   -1,    5,
       1,   -5,    5,   -1,   -5,    1,    3,   10,
      -3,  -10,   10,    3,  -10,   -3,    9,    9,
      -9,   -9,   -7,    7,    7,   -7,   -3,   12,
       3,  -12,   12,   -3,  -12,    3,    8,   17,
      -8,  -17,   17,    8,  -17,   -8,   17,   17,
     -17,  -17,    1,   19,   -1,  -19,   19,    1,
     -19,   -1,  -11,   16,   11,  -16,   16,  -11,
     -16,   11,   -6,   23,    6,  -23,   23,   -6,
     -23,    6,   18,   31,  -18,  -31,   31,   18,
     -31,  -18,    8,   32,   -8,  -32,   32,    8,
     -32,   -8,   33,   33,  -33,  -33,   -1,   36,
       1,  -36,   36,   -1,  -36,    1,  -23,   23,
      23,  -23,  -17,   31,   17,  -31,   31,  -17,
     -31,   17,   21,   54,  -21,  -54,   54,   21,
     -54,  -21,   37,   55,  -37,  -55,   55,   37,
     -55,  -37,  -12,   44,   12,  -44,   44,  -12,
     -44,   12,    8,   57,   -8,  -57,   57,    8,
     -57,   -8,   61,   61,  -61,  -61,   -5,   66,
       5,  -66,   66,   -5,  -66,    5,  -36,   45,
      36,  -45,   45,  -36,  -45,   36,  -29,   60,
      29,  -60,   60,  -29,  -60,   29,   45,   92,
     -45,  -92,   92,   45,  -92,  -45,   25,   93,
     -25,  -93,   93,   25,  -93,  -25,   71,   97,
     -71,  -97,   97,   71,  -97,  -71,  -22,   83,
      22,  -83,   83,  -22,  -83,   22,    7,  102,
      -7, -102,  102,    7, -102,   -7,  109,  109,
    -109, -109,  -68,   68,   68,  -68,  -57,   85,
      57,  -85,   85,  -57,  -85,   57,  -13,  120,
      13, -120,  120,  -13, -120,   13,  -49,  110,
      49, -110,  110,  -49, -110,   49, -104,  123,
     104, -123,  123, -104, -123,  104,   24,   24,
     -24,  -24,   46,   46,  -46,  -46,   84,   84,
     -84,  -84,
]};

const DT_1_4: IviDeltaCB = IviDeltaCB{ quad_radix: 11, data: &[
       0,    0,    5,    5,   -5,   -5,   -2,    7,
       2,   -7,    7,   -2,   -7,    2,   11,   11,
     -11,  -11,    3,   13,   -3,  -13,   13,    3,
     -13,   -3,   -9,    9,    9,   -9,   -4,   15,
       4,  -15,   15,   -4,  -15,    4,   11,   22,
     -11,  -22,   22,   11,  -22,  -11,   21,   21,
     -21,  -21,    2,   24,   -2,  -24,   24,    2,
     -24,   -2,  -14,   20,   14,  -20,   20,  -14,
     -20,   14,   23,   38,  -23,  -38,   38,   23,
     -38,  -23,   -8,   29,    8,  -29,   29,   -8,
     -29,    8,   11,   39,  -11,  -39,   39,   11,
     -39,  -11,   41,   41,  -41,  -41,   -1,   45,
       1,  -45,   45,   -1,  -45,    1,  -29,   29,
      29,  -29,  -22,   39,   22,  -39,   39,  -22,
     -39,   22,   27,   67,  -27,  -67,   67,   27,
     -67,  -27,   47,   69,  -47,  -69,   69,   47,
     -69,  -47,  -15,   56,   15,  -56,   56,  -15,
     -56,   15,   11,   71,  -11,  -71,   71,   11,
     -71,  -11,   76,   76,  -76,  -76,   -6,   83,
       6,  -83,   83,   -6,  -83,    6,  -45,   57,
      45,  -57,   57,  -45,  -57,   45,  -36,   75,
      36,  -75,   75,  -36,  -75,   36,   56,  115,
     -56, -115,  115,   56, -115,  -56,   31,  117,
     -31, -117,  117,   31, -117,  -31,   88,  122,
     -88, -122,  122,   88, -122,  -88,  -28,  104,
      28, -104,  104,  -28, -104,   28,  -85,   85,
      85,  -85,  -72,  106,   72, -106,  106,  -72,
    -106,   72,   30,   30,  -30,  -30,   58,   58,
     -58,  -58,  105,  105, -105, -105,
]};

const DT_1_5: IviDeltaCB = IviDeltaCB{ quad_radix: 12, data: &[
       0,    0,    6,    6,   -6,   -6,   -2,    8,
       2,   -8,    8,   -2,   -8,    2,   13,   13,
     -13,  -13,    4,   15,   -4,  -15,   15,    4,
     -15,   -4,  -11,   11,   11,  -11,   -5,   18,
       5,  -18,   18,   -5,  -18,    5,   13,   26,
     -13,  -26,   26,   13,  -26,  -13,   26,   26,
     -26,  -26,    2,   29,   -2,  -29,   29,    2,
     -29,   -2,  -16,   24,   16,  -24,   24,  -16,
     -24,   16,   28,   46,  -28,  -46,   46,   28,
     -46,  -28,   -9,   35,    9,  -35,   35,   -9,
     -35,    9,   13,   47,  -13,  -47,   47,   13,
     -47,  -13,   49,   49,  -49,  -49,   -1,   54,
       1,  -54,   54,   -1,  -54,    1,  -35,   35,
      35,  -35,  -26,   47,   26,  -47,   47,  -26,
     -47,   26,   32,   81,  -32,  -81,   81,   32,
     -81,  -32,   56,   83,  -56,  -83,   83,   56,
     -83,  -56,  -18,   67,   18,  -67,   67,  -18,
     -67,   18,   13,   86,  -13,  -86,   86,   13,
     -86,  -13,   91,   91,  -91,  -91,   -7,   99,
       7,  -99,   99,   -7,  -99,    7,  -54,   68,
      54,  -68,   68,  -54,  -68,   54,  -44,   90,
      44,  -90,   90,  -44,  -90,   44,  -33,  124,
      33, -124,  124,  -33, -124,   33, -103,  103,
     103, -103,  -86,  127,   86, -127,  127,  -86,
    -127,   86,   37,   37,  -37,  -37,   69,   69,
     -69,  -69,
]};

const DT_1_6: IviDeltaCB = IviDeltaCB{ quad_radix: 12, data: &[
       0,    0,    7,    7,   -7,   -7,   -3,   10,
       3,  -10,   10,   -3,  -10,    3,   16,   16,
     -16,  -16,    5,   18,   -5,  -18,   18,    5,
     -18,   -5,  -13,   13,   13,  -13,   -6,   21,
       6,  -21,   21,   -6,  -21,    6,   15,   30,
     -15,  -30,   30,   15,  -30,  -15,   30,   30,
     -30,  -30,    2,   34,   -2,  -34,   34,    2,
     -34,   -2,  -19,   28,   19,  -28,   28,  -19,
     -28,   19,   32,   54,  -32,  -54,   54,   32,
     -54,  -32,  -11,   41,   11,  -41,   41,  -11,
     -41,   11,   15,   55,  -15,  -55,   55,   15,
     -55,  -15,   57,   57,  -57,  -57,   -1,   63,
       1,  -63,   63,   -1,  -63,    1,  -40,   40,
      40,  -40,  -30,   55,   30,  -55,   55,  -30,
     -55,   30,   37,   94,  -37,  -94,   94,   37,
     -94,  -37,   65,   96,  -65,  -96,   96,   65,
     -96,  -65,  -21,   78,   21,  -78,   78,  -21,
     -78,   21,   15,  100,  -15, -100,  100,   15,
    -100,  -15,  106,  106, -106, -106,   -8,  116,
       8, -116,  116,   -8, -116,    8,  -63,   79,
      63,  -79,   79,  -63,  -79,   63,  -51,  105,
      51, -105,  105,  -51, -105,   51, -120,  120,
     120, -120,   43,   43,  -43,  -43,   80,   80,
     -80,  -80,
]};

const DT_1_7: IviDeltaCB = IviDeltaCB{ quad_radix: 12, data: &[
       0,    0,    8,    8,   -8,   -8,   -3,   11,
       3,  -11,   11,   -3,  -11,    3,   18,   18,
     -18,  -18,    5,   20,   -5,  -20,   20,    5,
     -20,   -5,  -15,   15,   15,  -15,   -7,   24,
       7,  -24,   24,   -7,  -24,    7,   17,   35,
     -17,  -35,   35,   17,  -35,  -17,   34,   34,
     -34,  -34,    3,   38,   -3,  -38,   38,    3,
     -38,   -3,  -22,   32,   22,  -32,   32,  -22,
     -32,   22,   37,   61,  -37,  -61,   61,   37,
     -61,  -37,  -13,   47,   13,  -47,   47,  -13,
     -47,   13,   17,   63,  -17,  -63,   63,   17,
     -63,  -17,   65,   65,  -65,  -65,   -1,   72,
       1,  -72,   72,   -1,  -72,    1,  -46,   46,
      46,  -46,  -35,   63,   35,  -63,   63,  -35,
     -63,   35,   43,  107,  -43, -107,  107,   43,
    -107,  -43,   75,  110,  -75, -110,  110,   75,
    -110,  -75,  -24,   89,   24,  -89,   89,  -24,
     -89,   24,   17,  114,  -17, -114,  114,   17,
    -114,  -17,  121,  121, -121, -121,  -72,   91,
      72,  -91,   91,  -72,  -91,   72,  -58,  120,
      58, -120,  120,  -58, -120,   58,   49,   49,
     -49,  -49,   92,   92,  -92,  -92,
]};

const DT_1_8: IviDeltaCB = IviDeltaCB{ quad_radix: 13, data: &[
       0,    0,    9,    9,   -9,   -9,   -3,   12,
       3,  -12,   12,   -3,  -12,    3,   20,   20,
     -20,  -20,    6,   23,   -6,  -23,   23,    6,
     -23,   -6,  -17,   17,   17,  -17,   -7,   27,
       7,  -27,   27,   -7,  -27,    7,   19,   39,
     -19,  -39,   39,   19,  -39,  -19,   39,   39,
     -39,  -39,    3,   43,   -3,  -43,   43,    3,
     -43,   -3,  -24,   36,   24,  -36,   36,  -24,
     -36,   24,   42,   69,  -42,  -69,   69,   42,
     -69,  -42,  -14,   53,   14,  -53,   53,  -14,
     -53,   14,   19,   71,  -19,  -71,   71,   19,
     -71,  -19,   73,   73,  -73,  -73,   -2,   80,
       2,  -80,   80,   -2,  -80,    2,  -52,   52,
      52,  -52,  -39,   70,   39,  -70,   70,  -39,
     -70,   39,   48,  121,  -48, -121,  121,   48,
    -121,  -48,   84,  124,  -84, -124,  124,   84,
    -124,  -84,  -27,  100,   27, -100,  100,  -27,
    -100,   27,  -81,  102,   81, -102,  102,  -81,
    -102,   81,   55,   55,  -55,  -55,  104,  104,
    -104, -104,
]};

const DT_2_1: IviDeltaCB = IviDeltaCB{ quad_radix: 7, data: &[
       0,    0,    2,    2,   -2,   -2,    0,    2,
       0,   -2,    2,    0,   -2,    0,    4,    4,
      -4,   -4,    0,    4,    0,   -4,    4,    0,
      -4,    0,   -4,    4,    4,   -4,   -2,    6,
       2,   -6,    6,   -2,   -6,    2,    4,    8,
      -4,   -8,    8,    4,   -8,   -4,    8,    8,
      -8,   -8,    0,   10,    0,  -10,   10,    0,
     -10,    0,   -4,    8,    4,   -8,    8,   -4,
      -8,    4,    8,   14,   -8,  -14,   14,    8,
     -14,   -8,   -2,   12,    2,  -12,   12,   -2,
     -12,    2,    4,   16,   -4,  -16,   16,    4,
     -16,   -4,   16,   16,  -16,  -16,    0,   18,
       0,  -18,   18,    0,  -18,    0,  -12,   12,
      12,  -12,   -8,   16,    8,  -16,   16,   -8,
     -16,    8,   10,   26,  -10,  -26,   26,   10,
     -26,  -10,   18,   28,  -18,  -28,   28,   18,
     -28,  -18,   -6,   22,    6,  -22,   22,   -6,
     -22,    6,    4,   28,   -4,  -28,   28,    4,
     -28,   -4,   30,   30,  -30,  -30,   -2,   32,
       2,  -32,   32,   -2,  -32,    2,  -18,   22,
      18,  -22,   22,  -18,  -22,   18,  -14,   30,
      14,  -30,   30,  -14,  -30,   14,   22,   46,
     -22,  -46,   46,   22,  -46,  -22,   12,   46,
     -12,  -46,   46,   12,  -46,  -12,   34,   48,
     -34,  -48,   48,   34,  -48,  -34,  -10,   40,
      10,  -40,   40,  -10,  -40,   10,    4,   50,
      -4,  -50,   50,    4,  -50,   -4,   54,   54,
     -54,  -54,  -34,   34,   34,  -34,  -28,   42,
      28,  -42,   42,  -28,  -42,   28,   -6,   60,
       6,  -60,   60,   -6,  -60,    6,   26,   76,
     -26,  -76,   76,   26,  -76,  -26,   42,   76,
     -42,  -76,   76,   42,  -76,  -42,  -24,   54,
      24,  -54,   54,  -24,  -54,   24,   14,   78,
     -14,  -78,   78,   14,  -78,  -14,   62,   82,
     -62,  -82,   82,   62,  -82,  -62,  -20,   74,
      20,  -74,   74,  -20,  -74,   20,    2,   88,
      -2,  -88,   88,    2,  -88,   -2,   92,   92,
     -92,  -92,  -52,   60,   52,  -60,   60,  -52,
     -60,   52,   52,  118,  -52, -118,  118,   52,
    -118,  -52,  -44,   74,   44,  -74,   74,  -44,
     -74,   44,   74,  118,  -74, -118,  118,   74,
    -118,  -74,   32,  118,  -32, -118,  118,   32,
    -118,  -32,  -12,  102,   12, -102,  102,  -12,
    -102,   12,  -40,   96,   40,  -96,   96,  -40,
     -96,   40,  -34,  118,   34, -118,  118,  -34,
    -118,   34,  -88,   88,   88,  -88,  -78,  104,
      78, -104,  104,  -78, -104,   78,   12,   12,
     -12,  -12,   22,   22,  -22,  -22,   42,   42,
     -42,  -42,   72,   72,  -72,  -72,
]};

const DT_2_2: IviDeltaCB = IviDeltaCB{ quad_radix: 9, data: &[
       0,    0,    3,    3,   -3,   -3,    0,    3,
       0,   -3,    3,    0,   -3,    0,    6,    6,
      -6,   -6,    3,    9,   -3,   -9,    9,    3,
      -9,   -3,   -3,    9,    3,   -9,    9,   -3,
      -9,    3,   -6,    6,    6,   -6,    6,   12,
      -6,  -12,   12,    6,  -12,   -6,   12,   12,
     -12,  -12,    0,   15,    0,  -15,   15,    0,
     -15,    0,   -9,   12,    9,  -12,   12,   -9,
     -12,    9,   15,   24,  -15,  -24,   24,   15,
     -24,  -15,   -6,   18,    6,  -18,   18,   -6,
     -18,    6,    6,   24,   -6,  -24,   24,    6,
     -24,   -6,   24,   24,  -24,  -24,    0,   27,
       0,  -27,   27,    0,  -27,    0,  -18,   18,
      18,  -18,  -12,   24,   12,  -24,   24,  -12,
     -24,   12,   15,   39,  -15,  -39,   39,   15,
     -39,  -15,   27,   42,  -27,  -42,   42,   27,
     -42,  -27,   -9,   33,    9,  -33,   33,   -9,
     -33,    9,    6,   42,   -6,  -42,   42,    6,
     -42,   -6,   45,   45,  -45,  -45,   -3,   51,
       3,  -51,   51,   -3,  -51,    3,  -27,   33,
      27,  -33,   33,  -27,  -33,   27,  -21,   45,
      21,  -45,   45,  -21,  -45,   21,   33,   69,
     -33,  -69,   69,   33,  -69,  -33,   18,   69,
     -18,  -69,   69,   18,  -69,  -18,   54,   72,
     -54,  -72,   72,   54,  -72,  -54,  -18,   63,
      18,  -63,   63,  -18,  -63,   18,    6,   78,
      -6,  -78,   78,    6,  -78,   -6,   81,   81,
     -81,  -81,  -51,   51,   51,  -51,  -42,   63,
      42,  -63,   63,  -42,  -63,   42,   -9,   90,
       9,  -90,   90,   -9,  -90,    9,   42,  114,
     -42, -114,  114,   42, -114,  -42,   63,  117,
     -63, -117,  117,   63, -117,  -63,  -36,   81,
      36,  -81,   81,  -36,  -81,   36,   21,  120,
     -21, -120,  120,   21, -120,  -21,   96,  123,
     -96, -123,  123,   96, -123,  -96,  -30,  111,
      30, -111,  111,  -30, -111,   30,  -78,   93,
      78,  -93,   93,  -78,  -93,   78,  -69,  114,
      69, -114,  114,  -69, -114,   69,   18,   18,
     -18,  -18,   33,   33,  -33,  -33,   63,   63,
     -63,  -63,  108,  108, -108, -108,
]};

const DT_2_3: IviDeltaCB = IviDeltaCB{ quad_radix: 10, data: &[
       0,    0,    4,    4,   -4,   -4,    0,    4,
       0,   -4,    4,    0,   -4,    0,    4,    8,
      -4,   -8,    8,    4,   -8,   -4,    8,    8,
      -8,   -8,   -8,    8,    8,   -8,   -4,   12,
       4,  -12,   12,   -4,  -12,    4,    8,   16,
      -8,  -16,   16,    8,  -16,   -8,   16,   16,
     -16,  -16,    0,   20,    0,  -20,   20,    0,
     -20,    0,  -12,   16,   12,  -16,   16,  -12,
     -16,   12,   -4,   24,    4,  -24,   24,   -4,
     -24,    4,   16,   32,  -16,  -32,   32,   16,
     -32,  -16,    8,   32,   -8,  -32,   32,    8,
     -32,   -8,   32,   32,  -32,  -32,    0,   36,
       0,  -36,   36,    0,  -36,    0,  -24,   24,
      24,  -24,  -16,   32,   16,  -32,   32,  -16,
     -32,   16,   20,   52,  -20,  -52,   52,   20,
     -52,  -20,   36,   56,  -36,  -56,   56,   36,
     -56,  -36,  -12,   44,   12,  -44,   44,  -12,
     -44,   12,    8,   56,   -8,  -56,   56,    8,
     -56,   -8,   60,   60,  -60,  -60,   -4,   64,
       4,  -64,   64,   -4,  -64,    4,  -36,   44,
      36,  -44,   44,  -36,  -44,   36,  -28,   60,
      28,  -60,   60,  -28,  -60,   28,   44,   92,
     -44,  -92,   92,   44,  -92,  -44,   24,   92,
     -24,  -92,   92,   24,  -92,  -24,   72,   96,
     -72,  -96,   96,   72,  -96,  -72,  -20,   84,
      20,  -84,   84,  -20,  -84,   20,    8,  100,
      -8, -100,  100,    8, -100,   -8,  108,  108,
    -108, -108,  -68,   68,   68,  -68,  -56,   84,
      56,  -84,   84,  -56,  -84,   56,  -12,  120,
      12, -120,  120,  -12, -120,   12,  -48,  108,
      48, -108,  108,  -48, -108,   48, -104,  124,
     104, -124,  124, -104, -124,  104,   24,   24,
     -24,  -24,   44,   44,  -44,  -44,   84,   84,
     -84,  -84,
]};

const DT_2_4: IviDeltaCB = IviDeltaCB{ quad_radix: 11, data: &[
       0,    0,    5,    5,   -5,   -5,    0,    5,
       0,   -5,    5,    0,   -5,    0,   10,   10,
     -10,  -10,    5,   15,   -5,  -15,   15,    5,
     -15,   -5,  -10,   10,   10,  -10,   -5,   15,
       5,  -15,   15,   -5,  -15,    5,   10,   20,
     -10,  -20,   20,   10,  -20,  -10,   20,   20,
     -20,  -20,    0,   25,    0,  -25,   25,    0,
     -25,    0,  -15,   20,   15,  -20,   20,  -15,
     -20,   15,   25,   40,  -25,  -40,   40,   25,
     -40,  -25,  -10,   30,   10,  -30,   30,  -10,
     -30,   10,   10,   40,  -10,  -40,   40,   10,
     -40,  -10,   40,   40,  -40,  -40,    0,   45,
       0,  -45,   45,    0,  -45,    0,  -30,   30,
      30,  -30,  -20,   40,   20,  -40,   40,  -20,
     -40,   20,   25,   65,  -25,  -65,   65,   25,
     -65,  -25,   45,   70,  -45,  -70,   70,   45,
     -70,  -45,  -15,   55,   15,  -55,   55,  -15,
     -55,   15,   10,   70,  -10,  -70,   70,   10,
     -70,  -10,   75,   75,  -75,  -75,   -5,   85,
       5,  -85,   85,   -5,  -85,    5,  -45,   55,
      45,  -55,   55,  -45,  -55,   45,  -35,   75,
      35,  -75,   75,  -35,  -75,   35,   55,  115,
     -55, -115,  115,   55, -115,  -55,   30,  115,
     -30, -115,  115,   30, -115,  -30,   90,  120,
     -90, -120,  120,   90, -120,  -90,  -30,  105,
      30, -105,  105,  -30, -105,   30,  -85,   85,
      85,  -85,  -70,  105,   70, -105,  105,  -70,
    -105,   70,   30,   30,  -30,  -30,   60,   60,
     -60,  -60,  105,  105, -105, -105,
]};

const DT_2_5: IviDeltaCB = IviDeltaCB{ quad_radix: 12, data: &[
       0,    0,    6,    6,   -6,   -6,    0,    6,
       0,   -6,    6,    0,   -6,    0,   12,   12,
     -12,  -12,    6,   12,   -6,  -12,   12,    6,
     -12,   -6,  -12,   12,   12,  -12,   -6,   18,
       6,  -18,   18,   -6,  -18,    6,   12,   24,
     -12,  -24,   24,   12,  -24,  -12,   24,   24,
     -24,  -24,    0,   30,    0,  -30,   30,    0,
     -30,    0,  -18,   24,   18,  -24,   24,  -18,
     -24,   18,   30,   48,  -30,  -48,   48,   30,
     -48,  -30,   -6,   36,    6,  -36,   36,   -6,
     -36,    6,   12,   48,  -12,  -48,   48,   12,
     -48,  -12,   48,   48,  -48,  -48,    0,   54,
       0,  -54,   54,    0,  -54,    0,  -36,   36,
      36,  -36,  -24,   48,   24,  -48,   48,  -24,
     -48,   24,   30,   78,  -30,  -78,   78,   30,
     -78,  -30,   54,   84,  -54,  -84,   84,   54,
     -84,  -54,  -18,   66,   18,  -66,   66,  -18,
     -66,   18,   12,   84,  -12,  -84,   84,   12,
     -84,  -12,   90,   90,  -90,  -90,   -6,   96,
       6,  -96,   96,   -6,  -96,    6,  -54,   66,
      54,  -66,   66,  -54,  -66,   54,  -42,   90,
      42,  -90,   90,  -42,  -90,   42,  -30,  126,
      30, -126,  126,  -30, -126,   30, -102,  102,
     102, -102,  -84,  126,   84, -126,  126,  -84,
    -126,   84,   36,   36,  -36,  -36,   66,   66,
     -66,  -66,
]};

const DT_2_6: IviDeltaCB = IviDeltaCB{ quad_radix: 12, data: &[
       0,    0,    7,    7,   -7,   -7,    0,    7,
       0,   -7,    7,    0,   -7,    0,   14,   14,
     -14,  -14,    7,   21,   -7,  -21,   21,    7,
     -21,   -7,  -14,   14,   14,  -14,   -7,   21,
       7,  -21,   21,   -7,  -21,    7,   14,   28,
     -14,  -28,   28,   14,  -28,  -14,   28,   28,
     -28,  -28,    0,   35,    0,  -35,   35,    0,
     -35,    0,  -21,   28,   21,  -28,   28,  -21,
     -28,   21,   35,   56,  -35,  -56,   56,   35,
     -56,  -35,  -14,   42,   14,  -42,   42,  -14,
     -42,   14,   14,   56,  -14,  -56,   56,   14,
     -56,  -14,   56,   56,  -56,  -56,    0,   63,
       0,  -63,   63,    0,  -63,    0,  -42,   42,
      42,  -42,  -28,   56,   28,  -56,   56,  -28,
     -56,   28,   35,   91,  -35,  -91,   91,   35,
     -91,  -35,   63,   98,  -63,  -98,   98,   63,
     -98,  -63,  -21,   77,   21,  -77,   77,  -21,
     -77,   21,   14,   98,  -14,  -98,   98,   14,
     -98,  -14,  105,  105, -105, -105,   -7,  119,
       7, -119,  119,   -7, -119,    7,  -63,   77,
      63,  -77,   77,  -63,  -77,   63,  -49,  105,
      49, -105,  105,  -49, -105,   49, -119,  119,
     119, -119,   42,   42,  -42,  -42,   77,   77,
     -77,  -77,
]};

const DT_2_7: IviDeltaCB = IviDeltaCB{ quad_radix: 12, data: &[
       0,    0,    8,    8,   -8,   -8,    0,    8,
       0,   -8,    8,    0,   -8,    0,   16,   16,
     -16,  -16,    8,   16,   -8,  -16,   16,    8,
     -16,   -8,  -16,   16,   16,  -16,   -8,   24,
       8,  -24,   24,   -8,  -24,    8,   16,   32,
     -16,  -32,   32,   16,  -32,  -16,   32,   32,
     -32,  -32,    0,   40,    0,  -40,   40,    0,
     -40,    0,  -24,   32,   24,  -32,   32,  -24,
     -32,   24,   40,   64,  -40,  -64,   64,   40,
     -64,  -40,  -16,   48,   16,  -48,   48,  -16,
     -48,   16,   16,   64,  -16,  -64,   64,   16,
     -64,  -16,   64,   64,  -64,  -64,    0,   72,
       0,  -72,   72,    0,  -72,    0,  -48,   48,
      48,  -48,  -32,   64,   32,  -64,   64,  -32,
     -64,   32,   40,  104,  -40, -104,  104,   40,
    -104,  -40,   72,  112,  -72, -112,  112,   72,
    -112,  -72,  -24,   88,   24,  -88,   88,  -24,
     -88,   24,   16,  112,  -16, -112,  112,   16,
    -112,  -16,  120,  120, -120, -120,  -72,   88,
      72,  -88,   88,  -72,  -88,   72,  -56,  120,
      56, -120,  120,  -56, -120,   56,   48,   48,
     -48,  -48,   88,   88,  -88,  -88,
]};

const DT_2_8: IviDeltaCB = IviDeltaCB{ quad_radix: 13, data: &[
       0,    0,    9,    9,   -9,   -9,    0,    9,
       0,   -9,    9,    0,   -9,    0,   18,   18,
     -18,  -18,    9,   27,   -9,  -27,   27,    9,
     -27,   -9,  -18,   18,   18,  -18,   -9,   27,
       9,  -27,   27,   -9,  -27,    9,   18,   36,
     -18,  -36,   36,   18,  -36,  -18,   36,   36,
     -36,  -36,    0,   45,    0,  -45,   45,    0,
     -45,    0,  -27,   36,   27,  -36,   36,  -27,
     -36,   27,   45,   72,  -45,  -72,   72,   45,
     -72,  -45,  -18,   54,   18,  -54,   54,  -18,
     -54,   18,   18,   72,  -18,  -72,   72,   18,
     -72,  -18,   72,   72,  -72,  -72,    0,   81,
       0,  -81,   81,    0,  -81,    0,  -54,   54,
      54,  -54,  -36,   72,   36,  -72,   72,  -36,
     -72,   36,   45,  117,  -45, -117,  117,   45,
    -117,  -45,   81,  126,  -81, -126,  126,   81,
    -126,  -81,  -27,   99,   27,  -99,   99,  -27,
     -99,   27,  -81,   99,   81,  -99,   99,  -81,
     -99,   81,   54,   54,  -54,  -54,  108,  108,
    -108, -108,
]};

const DT_3_1: IviDeltaCB = IviDeltaCB{ quad_radix: 11, data: &[
       0,    0,    2,    2,   -2,   -2,    0,    3,
       0,   -3,    3,    0,   -3,    0,    6,    6,
      -6,   -6,    0,    7,    0,   -7,    7,    0,
      -7,    0,   -5,    5,    5,   -5,    5,   -5,
      -5,    5,    6,   11,   -6,  -11,   11,    6,
     -11,   -6,    0,    8,    0,   -8,    8,    0,
      -8,    0,   11,   11,  -11,  -11,    0,   12,
       0,  -12,   12,    0,  -12,    0,   12,   17,
     -12,  -17,   17,   12,  -17,  -12,   17,   17,
     -17,  -17,    6,   18,   -6,  -18,   18,    6,
     -18,   -6,   -8,   11,    8,  -11,   11,   -8,
     -11,    8,    0,   15,    0,  -15,   15,    0,
     -15,    0,    0,   20,    0,  -20,   20,    0,
     -20,    0,   18,   25,  -18,  -25,   25,   18,
     -25,  -18,   11,   25,  -11,  -25,   25,   11,
     -25,  -11,   25,   25,  -25,  -25,  -14,   14,
      14,  -14,   14,  -14,  -14,   14,    0,   26,
       0,  -26,   26,    0,  -26,    0,  -11,   18,
      11,  -18,   18,  -11,  -18,   11,   -7,   22,
       7,  -22,   22,   -7,  -22,    7,   26,   34,
     -26,  -34,   34,   26,  -34,  -26,   18,   34,
     -18,  -34,   34,   18,  -34,  -18,   34,   34,
     -34,  -34,   11,   35,  -11,  -35,   35,   11,
     -35,  -11,    0,   29,    0,  -29,   29,    0,
     -29,    0,  -19,   22,   19,  -22,   22,  -19,
     -22,   19,  -15,   26,   15,  -26,   26,  -15,
     -26,   15,    0,   37,    0,  -37,   37,    0,
     -37,    0,   27,   44,  -27,  -44,   44,   27,
     -44,  -27,   36,   44,  -36,  -44,   44,   36,
     -44,  -36,   18,   44,  -18,  -44,   44,   18,
     -44,  -18,  -10,   33,   10,  -33,   33,  -10,
     -33,   10,   45,   45,  -45,  -45,    0,    0,
]};

const DT_3_2: IviDeltaCB = IviDeltaCB{ quad_radix: 13, data: &[
       0,    0,    0,    2,    0,   -2,    2,    0,
      -2,    0,    2,    2,   -2,   -2,    6,    6,
      -6,   -6,    0,    6,    0,   -6,    6,    0,
      -6,    0,   -4,    4,    4,   -4,   10,   -6,
     -10,    6,    0,  -12,    0,   12,   -6,  -12,
       6,  -12,   -6,   12,    6,   12,  -14,    0,
      14,    0,   12,   12,  -12,  -12,    0,  -18,
       0,   18,   14,  -12,  -14,   12,  -18,   -6,
      18,   -6,  -18,    6,   18,    6,  -10,  -18,
      10,  -18,  -10,   18,   10,   18,  -22,    0,
      22,    0,    0,  -24,    0,   24,  -22,  -12,
      22,  -12,  -22,   12,   22,   12,   -8,  -24,
       8,  -24,   -8,   24,    8,   24,  -26,   -6,
      26,   -6,  -26,    6,   26,    6,  -28,    0,
      28,    0,   20,   20,  -20,  -20,  -14,  -26,
      14,   26,  -30,  -12,   30,   12,  -10,  -32,
      10,   32,  -18,  -32,   18,   32,  -26,  -26,
      26,   26,  -34,  -20,   34,   20,  -38,  -12,
      38,   12,  -32,  -32,   32,   32,   32,   32,
     -22,  -40,  -34,  -34,   34,   34,
]};

const DT_3_3: IviDeltaCB = IviDeltaCB{ quad_radix: 13, data: &[
       0,    0,    0,    2,    0,   -2,    2,    0,
      -2,    0,    4,    4,   -4,   -4,   10,   10,
     -10,  -10,    0,   10,    0,  -10,   10,    0,
     -10,    0,   -6,    6,    6,   -6,   14,   -8,
     -14,    8,  -18,    0,   18,    0,   10,  -16,
     -10,   16,    0,  -24,    0,   24,  -24,   -8,
      24,   -8,  -24,    8,   24,    8,   18,   18,
     -18,  -18,   20,  -16,  -20,   16,  -14,  -26,
      14,  -26,  -14,   26,   14,   26,  -30,    0,
      30,    0,    0,  -34,    0,   34,  -34,   -8,
      34,   -8,  -34,    8,   34,    8,  -30,  -18,
      30,  -18,  -30,   18,   30,   18,  -10,  -34,
      10,  -34,  -10,   34,   10,   34,  -20,  -34,
      20,   34,  -40,    0,   40,    0,   30,   30,
     -30,  -30,  -40,  -18,   40,   18,    0,  -44,
       0,   44,  -16,  -44,   16,   44,  -36,  -36,
     -36,  -36,   36,   36,  -26,  -44,   26,   44,
     -46,  -26,   46,   26,  -52,  -18,   52,   18,
     -20,  -54,  -44,  -44,   44,   44,  -32,  -54,
     -46,  -46,  -46,  -46,   46,   46,
]};

const DT_3_4: IviDeltaCB = IviDeltaCB{ quad_radix: 13, data: &[
       0,    0,    0,    4,    0,   -4,    4,    0,
      -4,    0,    4,    4,   -4,   -4,   12,   12,
     -12,  -12,    0,   12,    0,  -12,   12,    0,
     -12,    0,   -8,    8,    8,   -8,    8,  -16,
      -8,   16,    0,  -24,    0,   24,  -24,   -8,
      24,   -8,  -24,    8,   24,    8,   20,  -16,
     -20,   16,  -28,    0,   28,    0,  -16,  -24,
      16,  -24,  -16,   24,   16,   24,    0,  -32,
       0,   32,  -28,  -16,   28,  -16,  -28,   16,
      28,   16,   -8,  -32,    8,  -32,  -32,   -8,
      32,   -8,  -32,    8,   32,    8,   -8,   32,
       8,   32,   24,   24,  -24,  -24,   24,  -24,
     -24,   24,  -20,  -32,   20,   32,  -40,    0,
      40,    0,  -40,  -16,   40,   16,    0,  -44,
       0,  -44,  -44,    0,   44,    0,    0,   44,
       0,   44,  -32,  -32,   32,   32,  -16,  -44,
      16,   44,  -24,  -44,  -44,  -24,   44,   24,
      24,   44,  -48,  -16,   48,   16,  -36,  -36,
     -36,  -36,   36,   36,   36,   36,  -20,  -52,
      40,   40,  -40,  -40,  -32,  -52,
]};

const DT_3_5: IviDeltaCB = IviDeltaCB{ quad_radix: 13, data: &[
       0,    0,    2,    2,   -2,   -2,    6,    6,
      -6,   -6,   12,   12,  -12,  -12,   20,   20,
     -20,  -20,   32,   32,  -32,  -32,   46,   46,
     -46,  -46,    0,    0,    0,    0,    0,    0,
       0,    0,    0,    0,    0,    0,    0,    0,
       0,    0,    0,    0,    0,    0,    0,    0,
       0,    0,    0,    0,    0,    0,    0,    0,
       0,    0,    0,    0,    0,    0,    0,    0,
       0,    0,    0,    0,    0,    0,    0,    0,
       0,    0,    0,    0,    0,    0,    0,    0,
       0,    0,    0,    0,    0,    0,    0,    0,
       0,    0,    0,    0,    0,    0,    0,    0,
       0,    0,    0,    0,    0,    0,    0,    0,
       0,    0,    0,    0,    0,    0,    0,    0,
       0,    0,    0,    0,    0,    0,    0,    0,
       0,    0,    0,    0,    0,    0,    0,    0,
       0,    0,    0,    0,    0,    0,    0,    0,
       0,    0,    0,    0,    0,    0,    0,    0,
       0,    0,    0,    0,    0,    0,    0,    0,
       0,    0,    0,    0,    0,    0,
]};

pub const IVI3_DELTA_CBS: [&IviDeltaCB; 24] = [
    &DT_1_1, &DT_1_2, &DT_1_3, &DT_1_4, &DT_1_5, &DT_1_6, &DT_1_7, &DT_1_8,
    &DT_2_1, &DT_2_2, &DT_2_3, &DT_2_4, &DT_2_5, &DT_2_6, &DT_2_7, &DT_2_8,
    &DT_3_1, &DT_3_2, &DT_3_3, &DT_3_4, &DT_3_5, &DT_3_5, &DT_3_5, &DT_3_5
];
