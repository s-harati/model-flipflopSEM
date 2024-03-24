package cooptest17coupling;

import java.util.Random;

public class AgentU {
	
	private double couplingCost; // coupling
	
	private Registrar registrar;
	private int decision;
	private double threshold;
	private int maxSums;
	
	public AgentU(int maxSums) {
		this.maxSums = maxSums; // added in version 2.2.2 : the maximum possible score for an action. will be used to scale decision interval down to [0,1]
		this.setDecision(0);
	}
	
	public void setCouplingCost(double couplingCost) {
		this.couplingCost = couplingCost;
		// debugging
		System.out.println("In AgentU.setCouplingCost. couplingCost: "+this.couplingCost);
		// end of debugging
	}
		
	public void setRegistrar(Registrar registrar) {
		this.registrar = registrar;
	}
	
	public void setThreshold(double thrMean, double thrSD, Random r) {
		this.threshold = thrMean + thrSD * r.nextGaussian();
	}
	
	public void setThreshold(double threshold) { // v4.1.0
		this.threshold = threshold;
	}
	
	public double getThreshold() {
		return this.threshold;
	}
	
	public void setDecision(int x) {
		this.decision = x;
	}
	
	public int getDecision() {
		return this.decision;
	}
	
	public void makeDecision() {
		// version 2.2.8 notes that an agent's previous decision does not reduce uniqueness
		// this.decision is updated in the end of this function
		// but before then, it holds the value of the previous decision
		double uniqueness = 1.0 / (1 + registrar.getNLast() - this.getDecision());
		// debugging
		System.out.println("in agentU, before decision. nLast:" + (registrar.getNLast()) + " 1/(1+nLast):" + (1/(1+registrar.getNLast())));
		double value = registrar.getNSum();
		this.setDecision( ((uniqueness * value / maxSums) > this.threshold + this.couplingCost) ? 1 : 0 ); // couplingCost added
		// debugging
		System.out.println("uniqueness:" + uniqueness + " value:" + value + " threshold:" + threshold + " uniqueness*value:" + (uniqueness*value) + " couplingCost:" + couplingCost + " decision:" + decision);
//		// test
		System.out.println("***AgentU.makeDecision()***(threshold: "+threshold+")");
		System.out.println("uniqueness: "+uniqueness+" , value: "+value+
				" , score: "+uniqueness*value/maxSums+
				" , couplingCost: "+couplingCost+
				" , decision: "+decision);
		System.out.println("***");
//		// end test
	}



}
