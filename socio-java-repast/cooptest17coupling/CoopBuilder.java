package cooptest17coupling;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import static java.nio.file.StandardCopyOption.*;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Objects;

import repast.simphony.context.Context;
import repast.simphony.dataLoader.ContextBuilder;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.parameter.Parameters;

public class CoopBuilder implements ContextBuilder<Object> {

	@Override
	public Context build(Context<Object> context) {
		// TODO Auto-generated method stub
		context.setId("cooptest17coupling");
		String modelVersion = "600"; // coupled with ecological model
		
		// get parameters
		Parameters params = RunEnvironment.getInstance().getParameters();
		int stepsPerEpisode = params.getInteger("steps_per_episode");
		int episodesPerRun = params.getInteger("episodes_per_run"); 
		int runsPerSet = params.getInteger("runs_per_set");
		int setsPerBatch = params.getInteger("sets_per_batch");
		int numLevelsVarIndependent = params.getInteger("num_levels_var_independent");
		int numLevelsVarResponse = params.getInteger("num_levels_var_response");
		double gamma = params.getDouble("gamma");		
		int agentUCount = params.getInteger("agentu_count");
		double agentUpopThrMean = params.getDouble("agentu_pop_thr_mean"); 
		double agentUpopThrSD = params.getDouble("agentu_pop_thr_sd");
		String objective = params.getString("objective"); // Calibration or Validation v4.0.0
		double epsilon = params.getDouble("epsilon");
		double alpha = params.getDouble("alpha");
		ArrayList<String> algorithmList = new ArrayList<String>(); // v4.0.0
			boolean algorithm_S = params.getBoolean("algorithm_S");
			if (algorithm_S) {algorithmList.add("TD_SARSA");}
			boolean algorithm_Q = params.getBoolean("algorithm_Q");
			if (algorithm_Q) {algorithmList.add("TD_QLearning");}
			boolean algorithm_expS = params.getBoolean("algorithm_expS");
			if (algorithm_expS) {algorithmList.add("TD_expectedSARSA");}
			boolean algorithm_S2 = params.getBoolean("algorithm_S2");
			if (algorithm_S2) {algorithmList.add("TD_DoubleSARSA");}
			boolean algorithm_Q2 = params.getBoolean("algorithm_Q2");
			if (algorithm_Q2) {algorithmList.add("TD_DoubleQLearning");}
			boolean algorithm_expS2 = params.getBoolean("algorithm_expS2");
			if (algorithm_expS2) {algorithmList.add("TD_DoubleExpectedSARSA");}
		
		// create output directory
		String strMean = String.format("%.2f",agentUpopThrMean); 
        strMean = strMean.substring(2,4);
        String strSD = String.format("%.2f",agentUpopThrSD); 
        strSD = strSD.substring(2,4);
        String strGamma = String.format("%.2f",gamma); 
        strGamma = strGamma.substring(0,1) + strGamma.substring(2,4);
        String strN = String.valueOf(agentUCount);
        String strEpsilon = String.format("%.2f",epsilon); 					// v4.0.0
        strEpsilon = strEpsilon.substring(0,1) + strEpsilon.substring(2,4);	//
        String strAlpha = String.format("%.2f",alpha); 						//
        strAlpha = strAlpha.substring(0,1) + strAlpha.substring(2,4);			//
        LocalDateTime date = LocalDateTime.now();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("uuuuMMdd_HHmmssSS");
        String strDate = date.format(formatter);
        strDate = strDate.substring(2,15);
        String strPathOut = "output/"+"coop"+modelVersion+
        		"_mu"+strMean+"_sd"+strSD+"_g"+strGamma+"_n"+strN+"_e"+strEpsilon+"_a"+strAlpha+"_"+strDate;
        if (Objects.equals(objective, "Calibration_writeThresholds")) {
    		strPathOut = strPathOut + "_thr";
        }
        File dirOut = new File(strPathOut);
        dirOut.mkdir();
        // copy parameters file in dirOut
        Path pathParams = Paths.get(context.getId() + ".rs/parameters.xml");
        Path pathOut = Paths.get(strPathOut+"/parameters.xml");
        try {
			Files.copy(pathParams, pathOut, REPLACE_EXISTING);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			System.out.println("EyVaaay (line80)");
			e.printStackTrace();
		}
        
        // AgentU
        int maxSums = agentUCount * stepsPerEpisode; // added in v2.2.2 to scale down AgentUdecision process to [0,1] for comparison with a [0,1] threshold.
		// define empty agentUArray
		AgentU agentUArray[] = new AgentU[agentUCount];
		// loop: agentUCount {add agentU(); add agentU to agentUArray} // later on, registrar will be assigned to the agents
		for (int i = 0; i < agentUCount ; i++) {
			AgentU agentU = new AgentU(maxSums);
			context.add(agentU);
			agentUArray[i] = agentU;
		}
		
		// AgentG
		AgentG agentG = new AgentG(agentUCount, stepsPerEpisode, episodesPerRun,
				numLevelsVarIndependent, numLevelsVarResponse,
				gamma, epsilon, alpha);
		context.add(agentG);
		
		// Registrar
		Registrar registrar = new Registrar(agentG, agentUArray, agentUCount);
		
		for (AgentU agentU : agentUArray) {
			agentU.setRegistrar(registrar);
		}

		agentG.setRegistrar(registrar);
		
		// RunMaker // in v4.0.0 runs batches with a list of algorithms
		RunMaker runMaker = new RunMaker(agentG, agentUArray, agentUCount, stepsPerEpisode, 
				agentUpopThrMean, agentUpopThrSD, /*runsPerSet,*/ strPathOut, 
				objective,  
				registrar, algorithmList);
		context.add(runMaker);
		

        
		RunEnvironment.getInstance().endAt(setsPerBatch);
		
		return context;

	}

}
