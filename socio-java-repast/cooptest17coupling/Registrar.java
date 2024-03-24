package cooptest17coupling;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.concurrent.TimeUnit;

public class Registrar {

	// for coupling - need to be changed for other runs
	private String folder_inbox_java = "your path to your 0-inboxes folder/0-inboxes/java/";
	private String folder_inbox_r    = "your path to your 0-inboxes folder/0-inboxes/r/";
	private String folder_archive_java  = "your path to your 0-inboxes folder/0-inboxes/archive-java/";
	private String folder_workshop_java  = "your path to your 0-inboxes folder/0-inboxes/workshop-java/";
	private String messageFileName; 
	private File myFile; // auxiliary
	
	private AgentG agentG;
	private AgentU[] agentUArray;
	private int nLast, nSum, agentUCount; // are we using agentUCount here? yes: in update :/ // shall we calculate densities here too
	
	private int[] agentUDecisionsArray; // added for coupling
	private int[] zeroArray; // auxiliary
	
	public Registrar(AgentG agentG, AgentU[] agentUArray, int agentUCount) {
		this.agentG = agentG;
		this.agentUArray = agentUArray;
		this.agentUCount = agentUCount;
		this.agentUDecisionsArray = new int[agentUCount]; // added for coupling
		this.resetStats(); // this should go after initiation of agentUDecisionsArray
		this.zeroArray = new int[agentUCount];
		for (int i=0; i < agentUCount; i++) {this.zeroArray[i] = 0;}
	}
	
	public int getNLast() {
		return this.nLast;
	}
	
	public int getNSum() {
		return this.nSum;
	}
	
	// this function is used in Exploring Starts algorithm
	public void setNLast(int startingNLast) {
		this.nLast = startingNLast;
	}
	
	// this function is used in Exploring Starts algorithm
	public void setNSum(int startingNSum) {
		this.nSum = startingNSum;
	}
	
	public void resetStats() {
		this.nLast = this.nSum = 0;
		int tempCounter = 0; // coupling
		for (AgentU agentU : agentUArray) { 	// added v5.0.0
			agentU.setDecision(0);				// this function is called at the beginning of each episode, hence no prior decision.
			agentUDecisionsArray[tempCounter] = 0; // coupling
			tempCounter++; // coupling
		}
	}
	
	public void step() {
		try {
			readCouplingMessage();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} // coupling
		// debugging
		System.out.println("In Registrar.step(). signal: "+agentG.getSignal());
		// end of debugging
		if (agentG.getSignal() == 0) {
			nSum += agentUCount;
			writeCouplingMessage(zeroArray);
		} else {
			// debugging
			//System.out.println("in registrar, before calling agentUs nLast:" + nLast);
						
			for (AgentU agentU : agentUArray) {
				agentU.makeDecision();
			}
			this.nLast = 0;
			int tempCounter = 0; int tempDecision; // coupling
			for (AgentU agentU : agentUArray) {
				tempDecision = agentU.getDecision();
				agentUDecisionsArray[tempCounter] = tempDecision; // coupling
				nLast += tempDecision;
				tempCounter++; // coupling
			}
			// debugging
			//System.out.println("in registrar, after calling agentUs nLast:" + nLast);
			nSum += nLast;
			writeCouplingMessage(agentUDecisionsArray);
		}
	}
	
	private void readCouplingMessage() throws IOException {
		// wait for file to be added to inbox
        File folder = new File(folder_inbox_java);
        String fileName , rFileName;
        int lenList , lenCheck;
        lenList = lenCheck = 0;
        boolean flag;
        File[] listOfFiles , checkFiles;

        // waiting block
        // note: folder is kept clean, so there is at most 1 file in it.
        listOfFiles = folder.listFiles();
        checkFiles = listOfFiles; // just because it insisted on initiating. bugger.
        //lenList = (listOfFiles==null) ? 0 : listOfFiles.length;    
        // the above line is commented out because
        // if the message comes too fast then it will be in inbox before the loop
        // and the loop will detect no change.
        lenList = 0;
        flag = false;
        while (!flag) {
        	checkFiles = folder.listFiles();
        	lenCheck = (checkFiles==null) ? 0 : checkFiles.length;
        	if (lenList != lenCheck) {
        		flag = true;                    
        		try {
					TimeUnit.SECONDS.sleep(0);
				} catch (InterruptedException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}                   
        	}
        }
        
        // reading block
        rFileName = checkFiles[0].getName(); // reminder: folder has only 1 file
        this.messageFileName = rFileName;
        // debugging
        System.out.println("In Registrar.readCouplingMessage. folder_inbox_java+rFileName: "+folder_inbox_java+rFileName);
        // end of debugging
        flag = false;
        while (!flag) {
        	myFile = new File(folder_inbox_java+rFileName);
        	if (myFile.exists()) {flag = true;}
        }
        FileReader myFileReader = new FileReader(myFile);
        BufferedReader myBR = new BufferedReader(myFileReader);
        String myLine1, myLine2
        String[] myLine1Split, myLine2Split
        // first line is costs info for U's
        myLine1 = myBR.readLine();
        // debugging
        System.out.println("In Registrar.readCouplingMessage. myLine1: "+myLine1);
        // end of debugging
        myLine1Split = myLine1.split(" ");
        double couplingCost, couplingRevenue, couplingScore;
        int tempCounter = 0; 
		for (AgentU agentU : agentUArray) {
			couplingCost = Double.parseDouble(myLine1Split[tempCounter]);
			agentU.setCouplingCost(couplingCost);
			tempCounter++; 
		}
        // second line is health score for G
        myLine2 = myBR.readLine();
        // debugging
        System.out.println("In Registrar.readCouplingMessage. myLine2: "+myLine2);
        // end of debugging
        myLine2Split = myLine2.split(" ");
        couplingScore = Double.parseDouble(myLine2Split[0]); // reminder: Line2 has only 1 number
        agentG.setCouplingScore(couplingScore);
		// close reader and clean inbox
        myBR.close();
        myFileReader.close();
        String archivePath = folder_archive_java + rFileName;
        flag = false;
        while(!flag) {
        	flag = myFile.renameTo(new File(archivePath));
        }
	}
	
	private void writeCouplingMessage(int[] arrayToWrite) {
		
		boolean flag;
		
		String fileName1 = folder_workshop_java + "j_" + this.messageFileName;
		// debugging
		System.out.println("In Registrar.writeCouplingMessage");
		System.out.println("Checking arrayToWrite...");
		for (int d:arrayToWrite) {System.out.print(d+" ");}
		System.out.println();
		// end of debugging
		try {
			writeArrayToFile(arrayToWrite, fileName1);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		// debugging
		File folder1 = new File(folder_workshop_java);
		File folder2 = new File(folder_inbox_r);
		System.out.println("Before move...");
		System.out.println("List of files of folder_workshop_java");
		for (int i=0; i < folder1.listFiles().length; i++) {
			System.out.print(folder1.listFiles()[i] + " ");
		}
		System.out.println();
		System.out.println("List of files of folder_inbox_r");
		for (int i=0; i < folder2.listFiles().length; i++) {
			System.out.print(folder2.listFiles()[i] + " ");
		}
		System.out.println();
		// end of debugging
		String fileName2 = folder_inbox_r + "j_" + this.messageFileName; 
		File myFile1 = new File(fileName1);
		flag = false;
		while(!flag) {
			flag = myFile1.renameTo(new File(fileName2));
		}
		// debugging
		System.out.println("After move...");
		System.out.println("List of files of folder_workshop_java");
		for (int i=0; i < folder1.listFiles().length; i++) {
			System.out.print(folder1.listFiles()[i] + " ");
		}
		System.out.println();
		System.out.println("List of files of folder_inbox_r");
		for (int i=0; i < folder2.listFiles().length; i++) {
			System.out.print(folder2.listFiles()[i] + " ");
		}
		System.out.println();
		// end of debugging
	}
	
	// auxiliary
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
