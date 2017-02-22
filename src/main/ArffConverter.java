package main;

import java.io.File;

import weka.core.Instances;
import weka.core.converters.ArffSaver;
import weka.core.converters.CSVLoader;

public class ArffConverter {

  private static final File inputFile = new File ("data/tCoursesSelected.csv");
  private static final File outputFile = new File ("data/instances.arff");

  
  public static void main(String[] args) throws Exception {

    // load CSV
    CSVLoader loader = new CSVLoader();
    loader.setSource(inputFile);
    Instances data = loader.getDataSet();
    
    // save ARFF
    ArffSaver saver = new ArffSaver();
    saver.setInstances(data);
    saver.setFile(outputFile);
    saver.writeBatch();
    
  }

}
