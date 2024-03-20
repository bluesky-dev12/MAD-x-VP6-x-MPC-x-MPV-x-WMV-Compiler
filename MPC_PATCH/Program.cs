using System;
using System.IO;

class Program
{
    static void Main(string[] args)
    {
        if (args.Length == 0)
        {
            Console.WriteLine("Error: Please provide the file path as an argument.");
            return;
        }

        string filePath = args[0]; // Path to the file

        if (!File.Exists(filePath))
        {
            Console.WriteLine($"Error: File '{filePath}' does not exist.");
            return;
        }

        string directoryPath = Path.GetDirectoryName(filePath);

        ReplaceAndRenameFiles(directoryPath);
    }

    static void ReplaceAndRenameFiles(string directoryPath)
    {
        try
        {
            string[] files = Directory.GetFiles(directoryPath, "*.mpc");

            foreach (string filePath in files)
            {
                string newFilePath = Path.ChangeExtension(filePath, ".mpv");
                ReplaceAndRenameFile(filePath, newFilePath);
            }

            Console.WriteLine("Replacement completed for all files in the directory.");
        }
        catch (Exception ex)
        {
            Console.WriteLine("Error: " + ex.Message);
        }
    }

    static void ReplaceAndRenameFile(string sourceFilePath, string destinationFilePath)
    {
        try
        {
            string fileContent = File.ReadAllText(sourceFilePath);

            // Replace "MPCh" with "MVhd" and "MPVh" with "MPCh" directly
            fileContent = fileContent.Replace("MPCh", "MVhd");
            fileContent = fileContent.Replace("MPVh", "MPCh");

            File.WriteAllText(destinationFilePath, fileContent);
            File.Delete(sourceFilePath); // Delete the original file

            Console.WriteLine($"Replacement completed for file: {Path.GetFileName(sourceFilePath)}");
            Console.WriteLine($"File renamed to: {Path.GetFileName(destinationFilePath)}");
        }
        catch (Exception ex)
        {
            Console.WriteLine("Error: " + ex.Message);
        }
    }
}