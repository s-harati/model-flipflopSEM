package cooptest17coupling;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Objects;
import java.util.Random;

//import cooptest2.AgentU;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.engine.schedule.ScheduledMethod;

public class RunMaker {
	
	private AgentG agentG;
	private AgentU[] agentUArray;
	private int agentUCount, stepsPerEpisode; // we are using these to create a norm for thresholds of AgentU objects. // removed to CoopBuilder
	private double agentUpopThrMean, agentUpopThrSD;
	private String strPathOut;
	private String objective;
	private Registrar registrar;
	private ArrayList<String> algorithmList; // v4.0.0
	
	private File fileThresholds; // v4.1.0 only used for validation
	private FileReader frThresholds; // v4.1.0
	private BufferedReader brThresholds; // v4.1.0
	private String strThresholdsLine; // v4.1.0
	private String[] strSplitThresholdsLine; // v4.1.0
	
	private double[] thresholds; // v4.2.0
	
	public RunMaker(AgentG agentG, AgentU[] agentUArray, int agentUCount, int stepsPerEpisode, 
			double agentUpopThrMean, double agentUpopThrSD, String strPathOut, 
			String objective, 
			Registrar registrar, ArrayList<String> algorithmList) {
		this.agentG = agentG;
		this.agentUArray = agentUArray;
		this.agentUCount = agentUCount;
		this.stepsPerEpisode = stepsPerEpisode;
		this.agentUpopThrMean = agentUpopThrMean; this.agentUpopThrSD = agentUpopThrSD;	
		this.strPathOut = strPathOut;
		this.objective = objective;
		this.registrar = registrar; 
		this.algorithmList = algorithmList; // v4.0.0
		
		this.thresholds = new double[agentUCount]; // will be saved on disk // v4.2.0 moved here
		
		if (Objects.equals(this.objective, "Validation") | Objects.equals(this.objective, "Calibration_readThresholds")
				| Objects.equals(this.objective, "Validation_randomBaseline")) {
			try {
				findThresholdsFile();
			} catch (IOException e) {
				e.printStackTrace();
			}
		} //v4.1.0
	}
	
	private void findThresholdsFile() throws IOException { //v4.1.0
		// v4.0.0 step1: look for calibration folder in "output" folder (note: each repast project has its own output folder)
		File folder = new File("output/");
		File[] listOfFiles = folder.listFiles();
		// v4.2.0
		String strMean = String.format("%.2f",agentUpopThrMean); 
        strMean = strMean.substring(2,4);
        String strSD = String.format("%.2f",agentUpopThrSD); 
        strSD = strSD.substring(2,4);
        String strN = String.valueOf(agentUCount);
		//
		String strPathCalibration = "";
		
		for (int i = 0 ; i < listOfFiles.length ; i++) {
			if (listOfFiles[i].getName().contains("_mu"+strMean) &
				listOfFiles[i].getName().contains("_sd"+strSD) &		// v4.2.0
				listOfFiles[i].getName().contains("_n"+strN) &
				listOfFiles[i].getName().contains("_thr")) 				// v4.2.0
			{
				strPathCalibration = "output/" + listOfFiles[i].getName() +"/";
				break;
			}
		}
		if (strPathCalibration.length()==0) {
			System.out.println("Calibration folder containing thresholds not found.");
			System.out.println("Run terminated.");
			RunEnvironment.getInstance().endRun();
		} 
		// v4.0.0 step2: look for thresholds file in calibration folder
		File pathCalibration = new File(strPathCalibration);
		File [] listOfCalibrationFiles = pathCalibration.listFiles();
		String strPathThresholds = "";
		for (int i = 0 ; i < listOfCalibrationFiles.length ; i++) {
			if (listOfCalibrationFiles[i].getName().contains("thresholds")) {
				strPathThresholds = strPathCalibration + listOfCalibrationFiles[i].getName();
				break;
			}
		}
		if (strPathThresholds.length()==0) {
			System.out.println("Thresholds file not found.");
			System.out.println("Run terminated.");
			RunEnvironment.getInstance().endRun();
		}
		// v4.1.0 open thresholds file
		this.fileThresholds = new File(strPathThresholds);
		this.frThresholds = new FileReader(this.fileThresholds);
		this.brThresholds = new BufferedReader(this.frThresholds);
	}
	
	@ScheduledMethod(start=1, interval = 1)
	public void makeRun() {
		
		int tickCounter = (int) RunEnvironment.getInstance().getCurrentSchedule().getTickCount();
		String outRefFileName = strPathOut + "/ref" + String.valueOf(tickCounter); // + ".txt";
		// debugging
		System.out.println("In RunMaker.makeRun(). tickCounter: "+tickCounter);
		// end of debugging
		
		///////////////////////////////////
		//      PART I - THRESHOLDS      //
		///////////////////////////////////
		
		if (Objects.equals(this.objective, "Validation") | Objects.equals(this.objective, "Calibration_readThresholds")
				| Objects.equals(this.objective, "Validation_randomBaseline")) // v4.2.0
		{ 
			// v4.0.0 step3: in thresholds file, read line number tickCounter
	        try {
			this.strThresholdsLine = this.brThresholds.readLine();
	        } catch (IOException e) {
	        		e.printStackTrace();
	        }
	        if (this.strThresholdsLine == null) {
	        		System.out.println("Error: null line read in thresholds file.");
	        		System.out.println("Run terminated.");
	        		RunEnvironment.getInstance().endRun();
	        }
	        this.strSplitThresholdsLine = this.strThresholdsLine.split(" ");
	        if (this.strSplitThresholdsLine.length != this.agentUCount) {
	        		System.out.println("Error: thresholds file does not match agentUCount");
	        		System.out.println("Run terminated.");
	        		RunEnvironment.getInstance().endRun();
	        }
			// v4.0.0 step4: set thresholds of U's
	        for (int i=0 ; i < agentUCount ; i++) {
	        		this.thresholds[i] = Double.parseDouble(strSplitThresholdsLine[i]); // v4.2.0
	        		this.agentUArray[i].setThreshold(this.thresholds[i]);
	        }
//	        // test
//	        for (AgentU agentU : agentUArray) {
//	        		System.out.print(agentU.getThreshold() + " ");
//	        }
//	        System.out.println();
//	        // end test
		} else if (Objects.equals(this.objective, "Calibration_writeThresholds")) // v4.2.0
		{
			// reset U's
			Random r = new Random();
			int thresIndex = 0;
			for (AgentU agentU : agentUArray) {
				agentU.setDecision(0);
				agentU.setThreshold(agentUpopThrMean, agentUpopThrSD, r);
				thresholds[thresIndex] = agentU.getThreshold();
				thresIndex++;
			}
		}
		
		///////////////////////////////////
		//         PART II - RUNS        //
		///////////////////////////////////
		
		if (Objects.equals(this.objective, "Validation")) {
			// step5: call agentG.runValidation()
			try {
				agentG.runValidation(outRefFileName);
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} 
		} else if (Objects.equals(this.objective, "Validation_randomBaseline")) {
			// step5: call agentG.runBaseline_Random()
			try {
				agentG.runBaseline_Random(outRefFileName);
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		} else if (this.objective.contains("Calibration")) // if this.objective is "Calibration"
		{ // v4.0.0
			//System.out.println("in RunMaker, tick"+tickCounter); // debugging
			for (String algorithm : algorithmList) { // v4.0.0
				agentG.setAlgorithm(algorithm); // v4.0.0
				// find policy: ask G to run its calibration algorithm
				agentG.runCalibrationAlgorithm();
				double[][] lastPolicy = agentG.getPolicy(); // needed for prediction

				double[] lastRewards = agentG.getRewards();
				int[][] lastStates = agentG.getStates();
				int[] lastActions = agentG.getActions();
				try {
					writeArrayToFile(lastRewards, (strPathOut + "/" + algorithm + "_rewards.txt"));
					writeArrayToFile(lastStates[0], (strPathOut + "/" + algorithm + "_states_inde.txt"));
					writeArrayToFile(lastStates[1], (strPathOut + "/" + algorithm + "_states_resp.txt"));
					writeArrayToFile(lastActions, (strPathOut + "/" + algorithm + "_actions.txt"));
					for (int i=0 ; i < lastPolicy.length ; i++) {					// v4.0.0 moved here
						writeArrayToFile(lastPolicy[i], (strPathOut + "/" + algorithm + "_policies.txt"));
					}
				} catch (IOException e) {
					// TODO Auto-generated catch block
					System.out.println("Problem!");
					e.printStackTrace();
				}
				//} v3.0.0 removing sets
			} // end for (algorithm)
		} // end if (objective is Calibration)
		try { 						// v4.2.0 moved here, out of if bloc, so that it is run for all cal and val
			writeArrayToFile(thresholds, (strPathOut + "/thresholds.txt"));
		} catch (IOException e) {
			// TODO Auto-generated catch block
			System.out.println("Problem!");
			e.printStackTrace();
		}

	} // end function
	
	private void writeArrayToFile(double[] arrayToWrite, String fileName) throws IOException {
		String outString = "";
		for (double x : arrayToWrite) {
			outString = outString + String.valueOf(x) + " ";
		}
		outString = outString.substring( 0 , outString.length() - 1 ); // to get rid of the last ","
		FileWriter fileWriter = new FileWriter(fileName, true);
		fileWriter.write(outString + "\n");
		fileWriter.close();		
	}
	
	// 
	private void writeArrayToFile(int[] arrayToWrite, String fileName) throws IOException { // overloading: same function as above, only with int[] arrayToWrite
		String outString = "";
		for (int x : arrayToWrite) {
			outString = outString + String.valueOf(x) + " ";
		}
		outString = outString.substring( 0 , outString.length() - 1 ); // to get rid of the last ","
		FileWriter fileWriter = new FileWriter(fileName, true);
		fileWriter.write(outString + "\n");
		fileWriter.close();		
	}


}
