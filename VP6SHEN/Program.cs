using System;
using System.IO;
using System.Text;

namespace Vp6Converter
{
    class Program
    {
        static void Main(string[] args)
        {
            string baseDir = Directory.GetCurrentDirectory();
            string[] vp6s = Directory.GetFiles(baseDir, "*.vp6");
            foreach (string vp6 in vp6s)
            {
                string tempFilePath = Path.Combine(baseDir, Path.GetFileNameWithoutExtension(vp6) + "_temp.vp6");
                using (FileStream fs = new FileStream(vp6, FileMode.Open, FileAccess.Read))
                using (BinaryReader reader = new BinaryReader(fs))
                {
                    using (FileStream outputFileStream = new FileStream(tempFilePath, FileMode.Create, FileAccess.Write))
                    using (BinaryWriter writer = new BinaryWriter(outputFileStream))
                    {
                        while (reader.BaseStream.Position < reader.BaseStream.Length)
                        {
                            byte[] buf = reader.ReadBytes(4);
                            string headerName = Encoding.UTF8.GetString(buf);
                            switch (headerName)
                            {
                                case "SCHl":
                                    headerName = "SHEN";
                                    break;
                                case "SCDl":
                                    headerName = "SDEN";
                                    break;
                                case "SCCl":
                                    headerName = "SCEN";
                                    break;
                            }
                            writer.Write(Encoding.UTF8.GetBytes(headerName));
                            buf = reader.ReadBytes(4);
                            writer.Write(buf);
                            int dataSize = BitConverter.ToInt32(buf, 0) - 8;
                            buf = reader.ReadBytes(dataSize);
                            writer.Write(buf);
                        }
                    }
                }
                File.Delete(vp6);
                File.Move(tempFilePath, vp6);
            }
        }
    }
}
