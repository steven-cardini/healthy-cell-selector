package main;

import java.io.File;

import weka.classifiers.trees.J48;
import weka.core.Instances;
import weka.core.converters.ConverterUtils.DataSource;

public class DecisionTreeBuilder {

  // https://weka.wikispaces.com/Use+Weka+in+your+Java+code
  
  private static final File inputFile = new File ("data/cell_instances.arff");
  
  public static void main(String[] args) throws Exception {

    DataSource source = new DataSource(inputFile.getAbsolutePath());
    Instances data = source.getDataSet();
    // setting class attribute if the data format does not provide this information
    if (data.classIndex() == -1)
      data.setClassIndex(data.numAttributes() - 1);
    
    String[] options = new String[1];
    options[0] = "-U";            // unpruned tree
    J48 tree = new J48();         // new instance of tree
    tree.setOptions(options);     // set the options
    tree.buildClassifier(data);   // build classifier
    
    System.out.println(tree.toString());
  }

}
