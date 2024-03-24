package cooptest17coupling;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;

public class AgentG {

	static int indexVarIndependent = 0, indexVarResponse = 1; // v3.0.0

	private double couplingScore; // coupling
	// this file is loaded in the coupled model. it is the result of the training of the model with 4k episodes
	private String fileMeanPolies = "your path to your mean policy file/meanPolicies-DXS-m7-s6-g010-n9-e1-a2.txt";
	// the file is read with function readTable() when the object agentG is constructed
	// immediately afterwards, the loaded policy is used to initialize Q.
	
	private Registrar registrar;
	private String algorithm;
	private int agentUCount;
	private int stepsPerEpisode, episodesPerRun;
	private int numLevelsVarIndependent, numLevelsVarResponse;
	private double gamma;
	// define policy as the probability of recommending signal=1 as action.
	private double[][] policy; // v3.0.0
	private Episode episode;
	private double epsilon, alpha; // v3.0.0
	private int numActions = 2; // in this project only binary actions are assumed. FOR ANY FUZZY DESIGN THE
								// WHOLE CODE SHOULD BE REVIZED!
	private int[] state; // version 3.0.0
	private int signal; // this is 'action' in RL terminology. don't ask why it's not called that here.
	private double targetCoopRatio = 0.5; // if the ratio of cooperating AgentU's reaches this value then the operation
											// ends with success.
	private double[][][] Q; // v3.0.0

	public AgentG(int agentUCount, int stepsPerEpisode, int episodesPerRun,
			int numLevelsVarIndependent, int numLevelsVarResponse, double gamma, double epsilon, double alpha) {

		this.agentUCount = agentUCount;
		this.stepsPerEpisode = stepsPerEpisode;
		this.episodesPerRun = episodesPerRun;
		this.numLevelsVarIndependent = numLevelsVarIndependent;
		this.numLevelsVarResponse = numLevelsVarResponse;
		this.gamma = gamma;

		this.policy = new double[numLevelsVarIndependent][numLevelsVarResponse]; // v3.0.0
		// for the coupled model - loading policy from given file:
		try {
			this.policy = readTable(fileMeanPolies,numLevelsVarIndependent,numLevelsVarResponse);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		this.episode = new Episode(stepsPerEpisode);
		// debugging
		// for(int i = 0 ; i < stepsPerEpisode ; i++) {System.out.print(episode.R[i]);}
		// System.out.println();
		this.state = new int[2]; // version 3.0.0
		this.Q = new double[numLevelsVarIndependent][numLevelsVarResponse][numActions]; // v3.0.0
		this.epsilon = epsilon;
		this.alpha = alpha;
	}
	
	private double[][] readTable(String path, int nr, int nc) throws IOException {        
        File myFile = new File(path);
        FileReader myFileReader = new FileReader(myFile);
        BufferedReader myBR = new BufferedReader(myFileReader);
        double[][] myTable = new double[nr][nc];
        int i = 0;
        String myLine;
        String[] myLineSplit;
        myLine = myBR.readLine();
        while (myLine != null) {
            myLineSplit = myLine.split(" ");
            for (int j = 0; j < nc ; j++) {
                myTable[i][j] = Double.parseDouble(myLineSplit[j]);
            }
            i++;
            myLine = myBR.readLine();
        }
        return(myTable);
    }
	
	public void setCouplingScore(double couplingScore) {
		this.couplingScore = couplingScore;
		// debugging
		System.out.println("In AgentG.setCouplingScore. couplingScore: "+this.couplingScore);
		// end of debugging
	}

	public void setRegistrar(Registrar registrar) {
		this.registrar = registrar;
	}
	
	public void setAlgorithm(String algorithm) { // v4.0.0
		this.algorithm = algorithm;
	}

	public int getSignal() {
		return this.signal;
	}

	public double[][] getPolicy() { // updated v3.0.0
		return this.policy;
	}

	public double[] getRewards() {
		return this.episode.R;
	}

	public int[][] getStates() { // updated v3.0.0
		return this.episode.S;
	}

	public int[] getActions() {
		return this.episode.A;
	}

	public void runCalibrationAlgorithm() {
		if (Objects.equals(algorithm, "TD_SARSA")) {
			runTD_SARSA();
		} else if (Objects.equals(algorithm, "TD_QLearning")) {
			runTD_QLearning();
			//testargmaxQ(); //OK
		} else if (Objects.equals(algorithm, "TD_expectedSARSA")) {
			runTD_expectedSARSA();
		} else if (Objects.equals(algorithm, "TD_DoubleQLearning")) {
			runTD_DoubleQLearning();
		} else if (Objects.equals(algorithm, "TD_DoubleSARSA")) {
			runTD_DoubleSARSA();
		} else if (Objects.equals(algorithm, "TD_DoubleExpectedSARSA")) {
			runTD_DoubleExpectedSARSA();
		}
		// System.out.println("still in runAlgorithm");
	}
	
	public void runBaseline_Random(String outRefFileName) throws IOException {
		//System.out.println("***starting runBaseline_Random()***");
		int nLast,nSum;
		double coopRatio;
		// loop
		for (int epi = 0 ; epi < this.episodesPerRun ; epi++) {
			episode.reset();
			registrar.resetStats();
			nLast = registrar.getNLast();
			nSum = registrar.getNSum();
			episode.R[0] = -1;
			// inner loop
			for (int timeStep=0 ; timeStep < stepsPerEpisode-1 ; timeStep++) {
				signal = (Math.random() > 0.5)? 1 : 0 ;
				//System.out.println("in G.runBaseline_Random()  t="+timeStep+"  signal="+signal); // debugging
				//System.out.println("   before registrar.setp() : nLast="+nLast+"  nSum="+nSum); // debugging
				registrar.step();
				nLast = registrar.getNLast();
				nSum = registrar.getNSum();
				//System.out.println("back in G.runBaseline_Random()"); // debugging
				//System.out.println("   after registrar.setp() : nLast="+nLast+"  nSum="+nSum); // debugging
				//System.out.println("   checking: registrar.nLast="+registrar.getNLast()+"  registrar.nSum="+registrar.getNSum());
				coopRatio = 1.0 * nLast / agentUCount;
				//System.out.println("coopRatio="+coopRatio);
				episode.R[timeStep+1] = (coopRatio >= targetCoopRatio)? 0 : coopRatio - 1;
				if (coopRatio >= targetCoopRatio) {break;}			
			} // end of inner loop
			
			// saving rewards in output file
			try {
				writeArrayToFile(episode.R, (outRefFileName + "random.txt"));
			} catch (IOException e) {
				// TODO Auto-generated catch block
				System.out.println("Problem!");
				e.printStackTrace();
			}
		} // end of outer loop
		//System.out.println("***ending runBaseline_Random()***");
	} // end of method runBaseline_Random()


	public void runValidation(String outRefFileName) throws IOException {
		double refLastReward, refSumReward, refMeanReward;
		StringBuilder refRewardString = new StringBuilder();
		long numAllPolicies = (long) Math.pow(2, stepsPerEpisode-1);
		for (long i = 0; i < numAllPolicies; i++) {
			String policyString = String.format("%" + (stepsPerEpisode-1) + "s", Long.toBinaryString(i)).replaceAll(" ",
					"0"); // binary string of length stepsPerEpisode
			registrar.resetStats();
			refSumReward = 0;
			refRewardString.setLength(0); // clearing the StringBuilder
			double coopRatio = 0.0; // v5.0.1 initiation here
			refLastReward = coopRatio - 1; 				
			refSumReward += refLastReward;				
			refRewardString.append(refLastReward + " ");
			for (int t = 0; t < stepsPerEpisode-1; t++) {
				signal = (policyString.charAt(t) == '1' ? 1 : 0);
				registrar.step();
				int nLast = registrar.getNLast(); 
				coopRatio = 1.0 * nLast / agentUCount; // v5.0.1 initiation before loop
				if (coopRatio >= targetCoopRatio) {
					break;
				}
				refLastReward = coopRatio - 1;				// v5.0.1 moved up
				refSumReward += refLastReward;				// v5.0.1
				refRewardString.append(refLastReward + " ");// v5.0.1
			}
			
			refMeanReward = refSumReward / stepsPerEpisode;
			// save all possible rewards (as validation reference)
			try {
				FileWriter sumsFileWriter = new FileWriter(outRefFileName + ".txt", true);
				sumsFileWriter.write(refMeanReward + " ");
				sumsFileWriter.close();
				FileWriter fullFileWriter = new FileWriter(outRefFileName + "full.txt", true);
				fullFileWriter.write(refRewardString + "\n");
				fullFileWriter.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}
	
	private int getArgmaxQ(int s0, int s1) { // added in v3.1.0 // s0: independent , s1:dependent
		int argmax = 0;
		for (int i=0 ; i<numActions ; i++) {
			argmax = (Q[s0][s1][i] > Q[s0][s1][argmax])? i : argmax;
		}
		return breakTiesRandomly(Q[s0][s1] , Q[s0][s1][argmax]);
	}
	
	private int getArgmaxQ(int s0, int s1, double[][][] Q) { // s0: independent , s1:dependent
		int argmax = 0;
		for (int i=0 ; i<numActions ; i++) {
			argmax = (Q[s0][s1][i] > Q[s0][s1][argmax])? i : argmax;
		}
		return breakTiesRandomly(Q[s0][s1] , Q[s0][s1][argmax]);
	}

	private int getArgmax(double[] vec) {
		int argmax = 0;
		for (int i=0 ; i<vec.length ; i++) {
			argmax = (vec[i] > vec[argmax])? i : argmax;
		}
		return breakTiesRandomly(vec , vec[argmax]);
	}
	
	private void testargmaxQ() { // OK
		// set Q
		for (int i=0 ; i < numLevelsVarIndependent ; i++) {
			for (int j=0 ; j < numLevelsVarResponse ; j++) {
				for (int k=0 ; k < numActions ; k++) {
					Q[i][j][k] = Math.abs(100*i + 10*j + k - 100);
					System.out.print("Q["+i+"]["+j+"]["+k+"]="+Q[i][j][k]+"    ");
				}
				System.out.println();
			}
		}
		// test
		int resTemp;
		for (int i=0 ; i < numLevelsVarIndependent ; i++) {
			for (int j=0 ; j < numLevelsVarResponse ; j++) {
				resTemp = getArgmaxQ(i,j);
				System.out.print("argmaxQ("+i+","+j+")="+resTemp+"    ");
				System.out.println("Q["+i+"]["+j+"][argmaxQ]="+Q[i][j][resTemp]);
			}
		}
	} 

	
	// v3.0.0 re-defining state as a pair of independent and response variables
	private void setState(int nSum, int nLast, int timeStep) {
		double coopRatio = 1.0 * nLast / agentUCount;
		state[indexVarIndependent] = (nSum == agentUCount * timeStep) ? numLevelsVarIndependent - 1
				: (int) (1.0 * (numLevelsVarIndependent) * nSum / (agentUCount * timeStep));
		state[indexVarResponse] = (coopRatio >= targetCoopRatio) ? numLevelsVarResponse - 1
				: (int) (1.0 * this.couplingScore * (numLevelsVarResponse - 1) * coopRatio / targetCoopRatio); // couplingScore added
//		// test
//		System.out.println("***setState(nSum="+nSum+",nLast="+nLast+",timeStep="+timeStep+")***");
//		System.out.println("calculated state: "+state[0]+","+state[1]);
//		System.out.println("*** end of setState ***");
//		// end test
	}

	private void runTD_SARSA() { // built in v3.0.0
		int timeStep, nSum, nLast, oldSignal;
		int[] oldState = new int[2];
		double coopRatio;
		// initialize Q
		initializeQ();
		// loop (episodes)
		for (int epi = 0; epi < episodesPerRun; epi++) {
			// initialize state
			state[indexVarIndependent] = 0;
			state[indexVarResponse] = 0;
			// set time to 0
			timeStep = 0;
			// reset registrar
			registrar.resetStats();
			episode.reset();
			episode.S[indexVarIndependent][timeStep] = state[indexVarIndependent];
			episode.S[indexVarResponse][timeStep] = state[indexVarResponse];
			// choose action (signal) based on state and policy derived from Q
			// update policy
			updatePolicy(state, epsilon);
			// update signal
			updateSignal();
			episode.A[timeStep] = signal;
			// oldState = state; // debugging: oops! this way future changes in state also change oldState!
			oldState[0] = state[0]; oldState[1] = state[1]; // this is the correct way!
			oldSignal = signal;
			// loop (steps)
			do {
//				// test
//				System.out.println("***runTD_SARSA()***");
//				System.out.println("old timeStep: "+timeStep);
//				System.out.println("oldState: "+state[0]+","+state[1]);
//				System.out.println("oldSignal: "+signal);
//				System.out.println("*");
//				// end test
				timeStep++;
//				// test
//				System.out.println("new timeStep: " + timeStep);
//				System.out.println("taking action: calling registrar to step()...");
//				// end test
				// take action (send signal), observe reward and new state
					// action
				registrar.step(); // signal is given to AgentU's and their response recorded.
				nLast = registrar.getNLast();
				nSum = registrar.getNSum();
				coopRatio = 1.0 * nLast / agentUCount;
					// reward
				episode.R[timeStep] = (coopRatio >= targetCoopRatio)? 0 : coopRatio - 1;
					// new state
				setState(nSum, nLast, timeStep);
				episode.S[indexVarIndependent][timeStep] = state[indexVarIndependent];
				episode.S[indexVarResponse][timeStep] = state[indexVarResponse];
				// choose new action (signal) based on state and policy derived from Q
				updatePolicy(state, epsilon);
				updateSignal();
				episode.A[timeStep] = signal;
				// update Q
//				// test
//				System.out.println("***still runTD_SARSA()***");
//				System.out.println("oldQ["+oldState[0]+"]["+oldState[1]+"]["+oldSignal+"]= "+Q[oldState[0]][oldState[1]][oldSignal]);
//				// end test
				Q[oldState[indexVarIndependent]][oldState[indexVarResponse]][oldSignal] +=
						alpha * (episode.R[timeStep] + // 20201225: change timestep-1 to timestep
								gamma * Q[state[indexVarIndependent]][state[indexVarResponse]][signal] -
								Q[oldState[indexVarIndependent]][oldState[indexVarResponse]][oldSignal]);
//				// test
//				System.out.println("newQ["+oldState[0]+"]["+oldState[1]+"]["+oldSignal+"]= "+Q[oldState[0]][oldState[1]][oldSignal]);
//				System.out.println("***");
//				// end test
				// update state and action (signal)
				// oldState = state; // debugging: oops! this way future changes in state also change oldState!
				oldState[0] = state[0]; oldState[1] = state[1]; // this is the correct way!
				oldSignal = signal;
				// until state is terminal (added in this implementation: or if stepsPerEpisode is reached)
			} while ((state[indexVarResponse] != (numLevelsVarResponse - 1)) & (timeStep < (stepsPerEpisode - 1)));
		}
	}

	private void runTD_QLearning() { // built in v3.1.0
		int timeStep, nSum, nLast, argmax;
		int[] oldState = new int[2];
		double coopRatio;
		// initialize Q
		initializeQ();
		// loop (episodes)
		for (int epi = 0; epi < episodesPerRun; epi++) {
			// initialize state
			state[indexVarIndependent] = 0;
			state[indexVarResponse] = 0;
			// set time to 0
			timeStep = 0;
			// reset registrar
			registrar.resetStats();
			episode.reset();
			episode.S[indexVarIndependent][timeStep] = state[indexVarIndependent];
			episode.S[indexVarResponse][timeStep] = state[indexVarResponse];
			oldState[0] = state[0]; oldState[1] = state[1]; // this is the correct way!
			// loop (steps)
			do {
//				// test
//				System.out.println("***runTD_QLearning()***");
//				System.out.println("old timeStep: "+timeStep);
//				System.out.println("oldState: "+state[0]+","+state[1]);
//				System.out.println("*");
//				// end test
				timeStep++; // 20201225 move to end of loop; adjust inside of loop accordingly // 20201227 moved here!
//				// choose action (signal) based on state and policy derived from Q
//				// update policy
				updatePolicy(state, epsilon);
//				// update signal // 20201227 note this is the action for PREVIOUS timeStep
				updateSignal();
				episode.A[timeStep-1] = signal; // 20201227 change timeStep to timeStep-1 because we have already upped timeStep before
//				// test
//				System.out.println("new timeStep: " + timeStep);
//				System.out.println("taking action: calling registrar to step()...");
//				// end test
				// take action (send signal), observe reward and new state
					// action
				registrar.step(); // signal is given to AgentU's and their response recorded.
				nLast = registrar.getNLast();
				nSum = registrar.getNSum();
				coopRatio = 1.0 * nLast / agentUCount;
					// reward
				episode.R[timeStep] = (coopRatio >= targetCoopRatio)? 0 : coopRatio - 1;
					// new state
				// here we are calculating state of the NEW TIMESTEP
				setState(nSum, nLast, timeStep);
				episode.S[indexVarIndependent][timeStep] = state[indexVarIndependent];
				episode.S[indexVarResponse][timeStep] = state[indexVarResponse];
//				// test
//				System.out.println("***still runTD_QLearning()***");
//				printQ();
//				System.out.println("state["+indexVarIndependent+"]="+state[indexVarIndependent]+
//						" , state["+indexVarResponse+"]="+state[indexVarResponse]);
//				// end test
				argmax = getArgmaxQ(state[indexVarIndependent],state[indexVarResponse]);
//				// test
//				System.out.println("argmax: "+argmax);
//				// end test
				Q[oldState[indexVarIndependent]][oldState[indexVarResponse]][signal] +=
						alpha * (episode.R[timeStep] + // 20201225 change timestep-1 to timestep
								gamma * Q[state[indexVarIndependent]][state[indexVarResponse]][argmax] -
								Q[oldState[indexVarIndependent]][oldState[indexVarResponse]][signal]);
//				// test
//				System.out.println("newQ["+oldState[0]+"]["+oldState[1]+"]["+signal+"]= "+Q[oldState[0]][oldState[1]][signal]);
//				System.out.println("***");
//				// end test
				// update state and action (signal)
				oldState[0] = state[0]; oldState[1] = state[1]; // this is the correct way!
				// until state is terminal (added in this implementation: or if stepsPerEpisode is reached)
			} while ((state[indexVarResponse] != (numLevelsVarResponse - 1)) & (timeStep < (stepsPerEpisode-1))); 
		}
	}
	
	private void runTD_expectedSARSA() { // 20201227 similar to Q-Learning, except instead of argmax uses expectation
		int timeStep, nSum, nLast;
		int[] oldState = new int[2];
		double coopRatio , expectedQ;
		// initialize Q
		initializeQ();
		// loop (episodes)
		for (int epi = 0; epi < episodesPerRun; epi++) {
			// initialize state
			state[indexVarIndependent] = 0;
			state[indexVarResponse] = 0;
			// set time to 0
			timeStep = 0;
			// reset registrar
			registrar.resetStats();
			episode.reset();
			episode.S[indexVarIndependent][timeStep] = state[indexVarIndependent];
			episode.S[indexVarResponse][timeStep] = state[indexVarResponse];
			oldState[0] = state[0]; oldState[1] = state[1]; // this is the correct way!
			// loop (steps)
			do {
//				// test
//				System.out.println("***runTD_expectedSARSA()***");
//				System.out.println("old timeStep: "+timeStep);
//				System.out.println("oldState: "+state[0]+","+state[1]);
//				System.out.println("*");
//				// end test
				timeStep++; // 20201225 move to end of loop; adjust inside of loop accordingly // 20201227 moved here!
//				// choose action (signal) based on state and policy derived from Q
//				// update policy
				updatePolicy(state, epsilon);
//				// update signal // 20201227 note this is the action for PREVIOUS timeStep
				updateSignal();
				episode.A[timeStep-1] = signal; // 20201227 change timeStep to timeStep-1 because we have already upped timeStep before
//				// test
//				System.out.println("new timeStep: " + timeStep);
//				System.out.println("taking action: calling registrar to step()...");
//				// end test
				// take action (send signal), observe reward and new state
					// action
				registrar.step(); // signal is given to AgentU's and their response recorded.
				nLast = registrar.getNLast();
				nSum = registrar.getNSum();
				coopRatio = 1.0 * nLast / agentUCount;
					// reward
				episode.R[timeStep] = (coopRatio >= targetCoopRatio)? 0 : coopRatio - 1;
					// new state
				// here we are calculating state of the NEW TIMESTEP
				setState(nSum, nLast, timeStep);
				episode.S[indexVarIndependent][timeStep] = state[indexVarIndependent];
				episode.S[indexVarResponse][timeStep] = state[indexVarResponse];
//				// test
//				System.out.println("***still runTD_expectedSARSA()***");
//				printQ();
//				System.out.println("state["+indexVarIndependent+"]="+state[indexVarIndependent]+
//						" , state["+indexVarResponse+"]="+state[indexVarResponse]);
//				// end test
				expectedQ = getExpectedQ(state[indexVarIndependent],state[indexVarResponse],epsilon); // epsilon is used: assuming greedy policies
//				// test
//				System.out.println("argmax: "+argmax);
//				// end test
				Q[oldState[indexVarIndependent]][oldState[indexVarResponse]][signal] +=
						alpha * (episode.R[timeStep] + // 20201225 change timestep-1 to timestep
								gamma * expectedQ - 
								Q[oldState[indexVarIndependent]][oldState[indexVarResponse]][signal]);
//				// test
//				System.out.println("newQ["+oldState[0]+"]["+oldState[1]+"]["+signal+"]= "+Q[oldState[0]][oldState[1]][signal]);
//				System.out.println("***");
//				// end test
				// update state and action (signal)
				oldState[0] = state[0]; oldState[1] = state[1]; // this is the correct way!
				// until state is terminal (added in this implementation: or if stepsPerEpisode is reached)
			} while ((state[indexVarResponse] != (numLevelsVarResponse - 1)) & (timeStep < (stepsPerEpisode-1))); 
		}
	}

	private void runTD_DoubleQLearning() { // built in v3.1.0
		int timeStep, nSum, nLast, argmax;
		int[] oldState = new int[2];
		double coopRatio;
		// initialize Q1 , Q2
		double[][][] Q1 = new double[numLevelsVarIndependent][numLevelsVarResponse][numActions];
		double[][][] Q2 = new double[numLevelsVarIndependent][numLevelsVarResponse][numActions];
		initializeQ(Q1);
		initializeQ(Q2);
		// loop (episodes)
		for (int epi = 0; epi < episodesPerRun; epi++) {
			// initialize state
			state[indexVarIndependent] = 0;
			state[indexVarResponse] = 0;
			// set time to 0
			timeStep = 0;
			// reset registrar
			registrar.resetStats();
			episode.reset();
			episode.S[indexVarIndependent][timeStep] = state[indexVarIndependent];
			episode.S[indexVarResponse][timeStep] = state[indexVarResponse];
			oldState[0] = state[0]; oldState[1] = state[1]; // this is the correct way!
			// loop (steps)
			do {
//				// test
//				System.out.println("***runTD_QLearning()***");
//				System.out.println("old timeStep: "+timeStep);
//				System.out.println("oldState: "+state[0]+","+state[1]);
//				System.out.println("*");
//				// end test
				timeStep++; // 20201225 move to end of loop; adjust inside of loop accordingly // 20201227 moved here!
//				// choose action (signal) based on state and policy derived from Q
//				// update policy
				updatePolicy(state, Q1, Q2, epsilon);
//				// update signal // 20201227 note this is the action for PREVIOUS timeStep
				updateSignal();
				episode.A[timeStep-1] = signal; // 20201227 change timeStep to timeStep-1 because we have already upped timeStep before
//				// test
//				System.out.println("new timeStep: " + timeStep);
//				System.out.println("taking action: calling registrar to step()...");
//				// end test
				// take action (send signal), observe reward and new state
					// action
				registrar.step(); // signal is given to AgentU's and their response recorded.
				nLast = registrar.getNLast();
				nSum = registrar.getNSum();
				coopRatio = 1.0 * nLast / agentUCount;
					// reward
				episode.R[timeStep] = (coopRatio >= targetCoopRatio)? 0 : coopRatio - 1;
					// new state
				// here we are calculating state of the NEW TIMESTEP
				setState(nSum, nLast, timeStep);
				episode.S[indexVarIndependent][timeStep] = state[indexVarIndependent];
				episode.S[indexVarResponse][timeStep] = state[indexVarResponse];
//				// test
//				System.out.println("***still runTD_QLearning()***");
//				printQ();
//				System.out.println("state["+indexVarIndependent+"]="+state[indexVarIndependent]+
//						" , state["+indexVarResponse+"]="+state[indexVarResponse]);
//				// end test
				// With 0.5 probability:
				if (Math.random() < 0.5) {
					// update Q1
					argmax = getArgmaxQ(state[indexVarIndependent],state[indexVarResponse],Q1);
					Q1[oldState[indexVarIndependent]][oldState[indexVarResponse]][signal] +=
							alpha * (episode.R[timeStep] + // 20201225 change timestep-1 to timestep
									gamma * Q2[state[indexVarIndependent]][state[indexVarResponse]][argmax] -
									Q1[oldState[indexVarIndependent]][oldState[indexVarResponse]][signal]);
				} else {
					// update Q2
					argmax = getArgmaxQ(state[indexVarIndependent],state[indexVarResponse],Q2);
					Q2[oldState[indexVarIndependent]][oldState[indexVarResponse]][signal] +=
							alpha * (episode.R[timeStep] + // 20201225 change timestep-1 to timestep
									gamma * Q1[state[indexVarIndependent]][state[indexVarResponse]][argmax] -
									Q2[oldState[indexVarIndependent]][oldState[indexVarResponse]][signal]);
				}
				
//				// test
//				System.out.println("argmax: "+argmax);
//				// end test
//				// test
//				System.out.println("newQ["+oldState[0]+"]["+oldState[1]+"]["+signal+"]= "+Q[oldState[0]][oldState[1]][signal]);
//				System.out.println("***");
//				// end test
				// update state and action (signal)
				oldState[0] = state[0]; oldState[1] = state[1]; // this is the correct way!
				// until state is terminal (added in this implementation: or if stepsPerEpisode is reached)
			} while ((state[indexVarResponse] != (numLevelsVarResponse - 1)) & (timeStep < (stepsPerEpisode-1))); 
		}
	}

	private void runTD_DoubleSARSA() { 
		int timeStep, nSum, nLast, oldSignal;
		int[] oldState = new int[2];
		double coopRatio;
		// initialize Q1 , Q2
		double[][][] Q1 = new double[numLevelsVarIndependent][numLevelsVarResponse][numActions];
		double[][][] Q2 = new double[numLevelsVarIndependent][numLevelsVarResponse][numActions];
		initializeQ(Q1);
		initializeQ(Q2);
		// loop (episodes)
		for (int epi = 0; epi < episodesPerRun; epi++) {
			// initialize state
			state[indexVarIndependent] = 0;
			state[indexVarResponse] = 0;
			// set time to 0
			timeStep = 0;
			// reset registrar
			registrar.resetStats();
			episode.reset();
			episode.S[indexVarIndependent][timeStep] = state[indexVarIndependent];
			episode.S[indexVarResponse][timeStep] = state[indexVarResponse];
			// choose action (signal) based on state and policy derived from Q
			// update policy
			updatePolicy(state, Q1, Q2, epsilon);
			// update signal
			updateSignal();
			episode.A[timeStep] = signal;
			// oldState = state; // debugging: oops! this way future changes in state also change oldState!
			oldState[0] = state[0]; oldState[1] = state[1]; // this is the correct way!
			oldSignal = signal;
			// loop (steps)
			do {
//				// test
//				System.out.println("***runTD_SARSA()***");
//				System.out.println("old timeStep: "+timeStep);
//				System.out.println("oldState: "+state[0]+","+state[1]);
//				System.out.println("oldSignal: "+signal);
//				System.out.println("*");
//				// end test
				timeStep++;
//				// test
//				System.out.println("new timeStep: " + timeStep);
//				System.out.println("taking action: calling registrar to step()...");
//				// end test
				// take action (send signal), observe reward and new state
					// action
				registrar.step(); // signal is given to AgentU's and their response recorded.
				nLast = registrar.getNLast();
				nSum = registrar.getNSum();
				coopRatio = 1.0 * nLast / agentUCount;
					// reward
				episode.R[timeStep] = (coopRatio >= targetCoopRatio)? 0 : coopRatio - 1;
					// new state
				setState(nSum, nLast, timeStep);
				episode.S[indexVarIndependent][timeStep] = state[indexVarIndependent];
				episode.S[indexVarResponse][timeStep] = state[indexVarResponse];
				// choose new action (signal) based on state and policy derived from Q
				updatePolicy(state, Q1, Q2, epsilon);
				updateSignal();
				episode.A[timeStep] = signal;
				// update Q
//				// test
//				System.out.println("***still runTD_DoubleSARSA()***");
//				System.out.println("oldQ["+oldState[0]+"]["+oldState[1]+"]["+oldSignal+"]= "+Q[oldState[0]][oldState[1]][oldSignal]);
//				// end test
				if (Math.random() < 0.5) {
					Q1[oldState[indexVarIndependent]][oldState[indexVarResponse]][oldSignal] +=
							alpha * (episode.R[timeStep] + // 20201225: change timestep-1 to timestep
									gamma * Q2[state[indexVarIndependent]][state[indexVarResponse]][signal] -
									Q1[oldState[indexVarIndependent]][oldState[indexVarResponse]][oldSignal]);
				} else {
					Q2[oldState[indexVarIndependent]][oldState[indexVarResponse]][oldSignal] +=
							alpha * (episode.R[timeStep] + // 20201225: change timestep-1 to timestep
									gamma * Q1[state[indexVarIndependent]][state[indexVarResponse]][signal] -
									Q2[oldState[indexVarIndependent]][oldState[indexVarResponse]][oldSignal]);					
				}
//				// test
//				System.out.println("newQ["+oldState[0]+"]["+oldState[1]+"]["+oldSignal+"]= "+Q[oldState[0]][oldState[1]][oldSignal]);
//				System.out.println("***");
//				// end test
				// update state and action (signal)
				// oldState = state; // debugging: oops! this way future changes in state also change oldState!
				oldState[0] = state[0]; oldState[1] = state[1]; // this is the correct way!
				oldSignal = signal;
				// until state is terminal (added in this implementation: or if stepsPerEpisode is reached)
			} while ((state[indexVarResponse] != (numLevelsVarResponse - 1)) & (timeStep < (stepsPerEpisode - 1)));
		}
	}

	private void runTD_DoubleExpectedSARSA() { 
		int timeStep, nSum, nLast;
		int[] oldState = new int[2];
		double coopRatio , expectedQ;
		// initialize Q1 , Q2
		double[][][] Q1 = new double[numLevelsVarIndependent][numLevelsVarResponse][numActions];
		double[][][] Q2 = new double[numLevelsVarIndependent][numLevelsVarResponse][numActions];
		initializeQ(Q1);
		initializeQ(Q2);
		// loop (episodes)
		for (int epi = 0; epi < episodesPerRun; epi++) {
			// initialize state
			state[indexVarIndependent] = 0;
			state[indexVarResponse] = 0;
			// set time to 0
			timeStep = 0;
			// reset registrar
			registrar.resetStats();
			episode.reset();
			episode.S[indexVarIndependent][timeStep] = state[indexVarIndependent];
			episode.S[indexVarResponse][timeStep] = state[indexVarResponse];
			oldState[0] = state[0]; oldState[1] = state[1]; // this is the correct way!
			// loop (steps)
			do {
//				// test
				System.out.println("***runTD_expectedSARSA()***");
				System.out.println("old timeStep: "+timeStep);
				System.out.println("oldState: "+state[0]+","+state[1]);
				System.out.println("*");
//				// end test
				timeStep++; // 20201225 move to end of loop; adjust inside of loop accordingly // 20201227 moved here!
//				// choose action (signal) based on state and policy derived from Q
//				// update policy
				updatePolicy(state, Q1, Q2, epsilon);
//				// update signal // 20201227 note this is the action for PREVIOUS timeStep
				updateSignal();
				episode.A[timeStep-1] = signal; // 20201227 change timeStep to timeStep-1 because we have already upped timeStep before
//				// test
				System.out.println("new timeStep: " + timeStep);
				System.out.println("taking action: calling registrar to step()...");
//				// end test
				// take action (send signal), observe reward and new state
					// action
				registrar.step(); // signal is given to AgentU's and their response recorded.
				nLast = registrar.getNLast();
				nSum = registrar.getNSum();
				coopRatio = 1.0 * nLast / agentUCount;
					// reward
				episode.R[timeStep] = (coopRatio >= targetCoopRatio)? 0 : coopRatio - 1;
					// new state
				// here we are calculating state of the NEW TIMESTEP
				setState(nSum, nLast, timeStep);
				episode.S[indexVarIndependent][timeStep] = state[indexVarIndependent];
				episode.S[indexVarResponse][timeStep] = state[indexVarResponse];
//				// test
//				System.out.println("***still runTD_expectedSARSA()***");
//				printQ();
//				System.out.println("state["+indexVarIndependent+"]="+state[indexVarIndependent]+
//						" , state["+indexVarResponse+"]="+state[indexVarResponse]);
//				// end test
				if (Math.random() < 0.5) {
					expectedQ = getExpectedQ(state[indexVarIndependent],state[indexVarResponse],Q2,epsilon); // epsilon is used: assuming greedy policies
					Q1[oldState[indexVarIndependent]][oldState[indexVarResponse]][signal] +=
							alpha * (episode.R[timeStep] + // 20201225 change timestep-1 to timestep
									gamma * expectedQ - 
									Q1[oldState[indexVarIndependent]][oldState[indexVarResponse]][signal]);
				} else {
					expectedQ = getExpectedQ(state[indexVarIndependent],state[indexVarResponse],Q1,epsilon); // epsilon is used: assuming greedy policies
					Q2[oldState[indexVarIndependent]][oldState[indexVarResponse]][signal] +=
							alpha * (episode.R[timeStep] + // 20201225 change timestep-1 to timestep
									gamma * expectedQ - 
									Q2[oldState[indexVarIndependent]][oldState[indexVarResponse]][signal]);					
				}
//				// test
//				System.out.println("newQ["+oldState[0]+"]["+oldState[1]+"]["+signal+"]= "+Q[oldState[0]][oldState[1]][signal]);
//				System.out.println("***");
//				// end test
				// update state and action (signal)
				oldState[0] = state[0]; oldState[1] = state[1]; // this is the correct way!
				// until state is terminal (added in this implementation: or if stepsPerEpisode is reached)
			} while ((state[indexVarResponse] != (numLevelsVarResponse - 1)) & (timeStep < (stepsPerEpisode-1))); 
		}
	}
	
			
	private int breakTiesRandomly(double[] vec, double val) { // tested OK
		int n = vec.length;
		int[] items = new int[n];		
		int itemsCount = 0;
		for (int i=0 ; i<n ; i++) {
			if (vec[i]==val) {
				items[itemsCount] = i;
				itemsCount++;
			}
		}
		int randIndex = (int)(Math.random()*itemsCount);
		return(items[randIndex]);
	}
	
	private double getExpectedQ(int s0, int s1, double epsilon) {
		// finding maximum Q value for the given state
		double qmax = Q[s0][s1][0];
		for (int i=1 ; i < numActions ; i++) {
			if (Q[s0][s1][i] > qmax) {qmax = Q[s0][s1][i];}
		}
		// finding repeated qmax values if any
		int numGreedy = 0;
		for (int i=0 ; i < numActions ; i++) {
			if (Q[s0][s1][i]==qmax) {numGreedy++;}
		}
		// probabilities of nongreedy and greedy actions in greedy policy
		double probNonGreedy = epsilon / numActions;
		double probGreedy = (1-epsilon)/numGreedy + probNonGreedy;
		// calculating expectedQ
		double coeff;
		double expectedQ = 0;
		for (int i=0 ; i < numActions ; i++) {
			coeff = (Q[s0][s1][i]==qmax) ? probGreedy : probNonGreedy ;
			expectedQ += coeff * Q[s0][s1][i];
		}
		return expectedQ;
	}
	
	private double getExpectedQ(int s0, int s1, double[][][] Q, double epsilon) {
		// finding maximum Q value for the given state
		double qmax = Q[s0][s1][0];
		for (int i=1 ; i < numActions ; i++) {
			if (Q[s0][s1][i] > qmax) {qmax = Q[s0][s1][i];}
		}
		// finding repeated qmax values if any
		int numGreedy = 0;
		for (int i=0 ; i < numActions ; i++) {
			if (Q[s0][s1][i]==qmax) {numGreedy++;}
		}
		// probabilities of nongreedy and greedy actions in greedy policy
		double probNonGreedy = epsilon / numActions;
		double probGreedy = (1-epsilon)/numGreedy + probNonGreedy;
		// calculating expectedQ
		double coeff;
		double expectedQ = 0;
		for (int i=0 ; i < numActions ; i++) {
			coeff = (Q[s0][s1][i]==qmax) ? probGreedy : probNonGreedy ;
			expectedQ += coeff * Q[s0][s1][i];
		}
		return expectedQ;
	}

	private void updateSignal() { // built in v3.0.0 calculates signal based on policy & state
		signal = (Math.random() < policy[state[indexVarIndependent]][state[indexVarResponse]]) ? 1 : 0;
//		// test
//		System.out.println("***updateSignal()***");
//		System.out.println("signal: " + signal);
//		System.out.println("***");
//		// end test
	}
	
	private void updateSignal(double[][] policy) {
//		// debugging
//		System.out.println("debugging updateSignal(double[][] policy)");
//		System.out.print("state[indexVarIndependent]="+state[indexVarIndependent]);
//		System.out.println("    state[indexVarResponse]="+state[indexVarResponse]);
//		System.out.println("end of debugging updateSignal(double[][] policy)");
//		//
		signal = (Math.random() < policy[state[indexVarIndependent]][state[indexVarResponse]]) ? 1 : 0;
	}

	private void updatePolicy(int[] tempState) { // built in v3.0.0 calculates policy based on Q and state. probabilistic.
		int s0 = tempState[indexVarIndependent];
		int s1 = tempState[indexVarResponse];
		double qa0 = Q[s0][s1][0];
		double qa1 = Q[s0][s1][1];
		policy[s0][s1] = 0.5 + 0.5 * (qa1 - qa0) / (Math.abs(qa0) + Math.abs(qa1));
	}

	private void updatePolicy(int[] tempState, double epsilon) { // overloading. epsilon-soft
		double res;
		int s0 = tempState[indexVarIndependent];
		int s1 = tempState[indexVarResponse];
		int a_star = getArgmaxQ(s0,s1);
		res = (a_star==1)? 1 - epsilon + epsilon/numActions : epsilon/numActions;
//		if (Math.random() < epsilon) {
//			// exploration
//			res = Math.random();
//		} else {
//			// exploitation
//			res = (Q[s0][s1][0] < Q[s0][s1][1]) ? 1.0 : 0;
//		}
		policy[s0][s1] = res;
//		// test
//		System.out.println("***updatePolicy(state={"+s0+","+s1+"},epsilon="+epsilon+")***");
//		System.out.print("Q["+s0+"]["+s1+"][0]= " + Q[s0][s1][0] + " ; ");
//		System.out.println("Q["+s0+"]["+s1+"][1]= " + Q[s0][s1][1] + " ; ");
//		System.out.println("policy["+s0+"]["+s1+"]: " + policy[s0][s1]);
//		System.out.println("***");
//		// end test
	}

	private void updatePolicy(int[] tempState, double[][][] Q, double epsilon) { // overloading. epsilon-soft
		double res;
		int s0 = tempState[indexVarIndependent];
		int s1 = tempState[indexVarResponse];
		int a_star = getArgmaxQ(s0,s1,Q);
		res = (a_star==1)? 1 - epsilon + epsilon/numActions : epsilon/numActions;
		policy[s0][s1] = res;
	}
	
	private void updatePolicy(int[] tempState, double[][][] Q1, double[][][] Q2, double epsilon) { // overloading. epsilon-soft
		double res;
		int s0 = tempState[indexVarIndependent];
		int s1 = tempState[indexVarResponse];
		double[] vec = new double[((Q1[s0][s1].length) + (Q2[s0][s1].length))];
//		// debugging
//		System.out.println("***debugging updatePolicy(tempState,Q1,Q2,epsilon)***");
//		System.out.println("Q1[s0][s1].length="+Q1[s0][s1].length+" Q2[s0][s1].length="+Q2[s0][s1].length+" vec.length="+vec.length);
//		//
		for (int i=0; i<Q1[s0][s1].length; i++) {
//			// debugging
//			System.out.println("i="+i);
//			//
			vec[i] = Q1[s0][s1][i];}
		for (int j=0; j<Q2[s0][s1].length; j++) {vec[j+Q1[s0][s1].length] = Q2[s0][s1][j];}
		int a_star = getArgmax(vec) % numActions; // note: numActions=2. if argmax(vec) is 1 or 3 it means in Q1 or Q2 the argmax points to action1
		res = (a_star==1)? 1 - epsilon + epsilon/numActions : epsilon/numActions;
		policy[s0][s1] = res;
//		//
//		System.out.println("***end of debugging updatePolicy(tempState,Q1,Q2,epsilon)***");
//		//
	}
	
	// this function is modified in the coupled model
		// to initialize Q based on the imported policy
		// such that in each nonterminal state i,j 
		// Q of action0 is -0.5 and
		// Q of action1 is 0.5(1-1/p) where p is policy of i,j
	// although it is not used, because a double-learning algorithm is used in the coupled model.
	private void initializeQ() { // built in v3.0.0 sets random values to (nonterminal S,A), 0 to (terminal S,A)
		for (int i = 0; i < numLevelsVarIndependent; i++) {
			for (int j = 0; j < numLevelsVarResponse; j++) {
//				for (int k = 0; k < numActions; k++) {
//					Q[i][j][k] = (j == (numLevelsVarResponse - 1)) ? 0 : - Math.random();
//				}
				Q[i][j][0] = (j == (numLevelsVarResponse - 1)) ? 0 : -0.5;
				Q[i][j][1] = (j == (numLevelsVarResponse - 1)) ? 0 : 0.5*(1 - 1/this.policy[i][j]);
			}
		}
//		// test
//		System.out.println("***initializeQ()***");
//		for (int i = 0; i < numLevelsVarIndependent; i++) {
//			for (int j = 0; j < numLevelsVarResponse; j++) {
//				for (int k = 0; k < numActions; k++) {
//					System.out.print("Q["+i+"]["+j+"]["+k+"]= "+Q[i][j][k]+" , ");
//				} System.out.println();
//			} 
//		}
//		System.out.println("***");
//		// end test
	}
	
	// this function is modified in the coupled model
	// to initialize Q based on the imported policy
	// such that in each nonterminal state i,j 
	// Q of action0 is -0.5 and
	// Q of action1 is 0.5(1-1/p) where p is policy of i,j
	private void initializeQ(double[][][] Q) { 
		for (int i = 0; i < numLevelsVarIndependent; i++) {
			for (int j = 0; j < numLevelsVarResponse; j++) {
//				for (int k = 0; k < numActions; k++) {
//					Q[i][j][k] = (j == (numLevelsVarResponse - 1)) ? 0 : - Math.random();
//				}
				Q[i][j][0] = (j == (numLevelsVarResponse - 1)) ? 0 : -0.5;
				Q[i][j][1] = (j == (numLevelsVarResponse - 1)) ? 0 : 0.5*(1 - 1/this.policy[i][j]);
			}
		}
	}

	private void printQ() {
		System.out.println("*** printQ ***");
		for (int i = 0; i < numLevelsVarIndependent; i++) {
			for (int j = 0; j < numLevelsVarResponse; j++) {
				for (int k = 0; k < numActions; k++) {
					System.out.print("Q["+i+"]["+j+"]["+k+"]= "+String.format("%.2f", Q[i][j][k])+" , ");
				} System.out.print(" *** ");
			} System.out.println();
		} System.out.println("*** end of printQ ***");
	}

	private void initializeQ(double x) { // built in v3.0.0 overloading. sets fixed value x to (nonterminal S,A)
		for (int i = 0; i < numLevelsVarIndependent; i++) {
			for (int j = 0; j < numLevelsVarResponse; j++) {
				for (int k = 0; k < numActions; k++) {
					Q[i][j][k] = (j == (numLevelsVarResponse - 1)) ? 0 : x;
				}
			}
		}
	}
	
	
	// copied from RunMaker to help with runBaseline_Random()
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

class Episode {

	int[][] S; // v3.0.0
	int[] A;
	double[] R;
	int stepsPerEpisode;

	public Episode(int stepsPerEpisode) {
		this.stepsPerEpisode = stepsPerEpisode;
		this.S = new int[2][stepsPerEpisode]; // v3.0.0
		this.A = new int[stepsPerEpisode];
		this.R = new double[stepsPerEpisode]; // note: in Sutton's book indices of R are 1..T . in this code they are
												// 0..(T-1). note: initial values are 0.0 // v3.0.0 revoke note! back to Sutton's!
	}
	
	public void reset() { // built v3.0.0
		for (int i=0 ; i < stepsPerEpisode ; i++) {
			S[0][i] = -1; // to identify early end of episode
			S[1][i] = -1; // "
			A[i] = -1;    // "
			R[i] = 0; 
		}
		R[0] = -1;
	}
}