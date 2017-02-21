package main;

import java.io.FileReader;
import java.io.IOException;
import com.opencsv.CSVReader;

public class DataImport {

  private static final String dataFile = "data/tCoursesSelected.csv";
  
  public static void main(String[] args) throws IOException {
    CSVReader reader = new CSVReader(new FileReader(dataFile));
    String [] nextLine;
    while ((nextLine = reader.readNext()) != null) {
       // nextLine[] is an array of values from the line
      
       System.out.println(nextLine[0]+" "+nextLine[1]+" "+nextLine[2]);
    }
  }
  
}
