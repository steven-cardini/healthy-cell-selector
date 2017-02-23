package main;

import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.util.Iterator;

import com.opencsv.CSVReader;
import com.opencsv.CSVWriter;

public class CsvCondenser {

  private static final File inputFile = new File ("data/tCoursesSelected_midInCol.csv");
  private static final File outputFile = new File ("data/cell_instances.csv");
  
  private static final int featureAmount = 31;
  private static final int firstFeatureIndex = 7;
  private static final int classIndex = 38;
  
  public static void main(String[] args) throws Exception {
    
    CSVReader reader = new CSVReader(new FileReader(inputFile));
    Iterator<String[]> it = reader.iterator();
    CSVWriter writer = new CSVWriter(new FileWriter(outputFile));
    
    // write column titles to output file
    String[] titles = it.next();
    String[] relevantTitles = new String[featureAmount+1];
    for (int t_i=firstFeatureIndex; t_i < titles.length; t_i++) {
      relevantTitles[t_i-firstFeatureIndex] = titles[t_i];
    }
    writer.writeNext(relevantTitles); // directly output titles
    
    // initialize variables
    String[] currentDataset = null, newDataset = null;
    String currentClass = null, newClass = null;
    int datasetId = 0;
    
    while (it.hasNext()) {
      datasetId++;
      System.out.println("Dataset " + datasetId);
      
      if (newDataset == null) {
        currentDataset = it.next();
        currentClass = currentDataset[classIndex];
      } else {
        currentDataset = newDataset;
        currentClass = newClass;
      }
      
      double[] condensedData = new double[featureAmount];
      for (int i = 0; i<featureAmount; i++) condensedData[i] = Double.parseDouble(currentDataset[i+firstFeatureIndex]);
      int size = 1;
            
      newDataset = it.next();
      newClass = newDataset[classIndex];
    
      while ( newDataset[0].equals(currentDataset[0]) && newDataset[1].equals(currentDataset[1]) && newDataset[2].equals(currentDataset[2]) 
           && newDataset[3].equals(currentDataset[3]) && newDataset[4].equals(currentDataset[4]) && newDataset[6].equals(currentDataset[6]) )
      {
        size++;
        if (!newClass.equals(currentClass)) currentClass = "FALSE";
        
        for (int i = 0; i<featureAmount; i++) condensedData[i] = condensedData[i] + Double.parseDouble(newDataset[i+firstFeatureIndex]);      
        
        if (it.hasNext()) {
          newDataset = it.next();
          newClass = newDataset[classIndex];
        } else {
          break;
        }
      }
      
      // calculate average values
      for (int i = 0; i<featureAmount; i++) condensedData[i] = condensedData[i] / size;
      
      // output dataset to new CSV
      String[] outputData = new String[featureAmount+1];
      for (int o_i=0; o_i<featureAmount; o_i++) outputData[o_i] = Double.toString(condensedData[o_i]);
      outputData[outputData.length-1] = currentClass;
      writer.writeNext(outputData);
    }
    
    reader.close();
    writer.close();
  }

}
