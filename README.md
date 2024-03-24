# model-flipflopSEM

FlipFlop: a Social-Ecological Model

Developed by Saeed Harati-Asl, Liliana Perez and Roberto Molowny-Horas

This is a coupled social-ecological model, prepared to run hypothetical experiments of intervention in an ecosystem under disturbance. The model serves as a decision support tool for policy makers as well as an example for researchers and modelling professionals. Detailed description of the model and its use is presented in a scientific paper that is being assessed for publication. This README file will be updated upon publication of the paper.

The model is built by coupling two independently developed models that were written in Java (repast-simphony) and R. The two parts of the model interact with each other through a mechanism that we named flip flop. The two models take turns and send messages to one-another.

To run the coupled model: First, create the folder structure that is used in the flip-flop mechanism (use the link below) Next, download or prepare data required for running each part of the model (use the link below) Finally, enter the paths of the above-said folders and datasets in the codes. In R codes, these paths should be entered in identified lines at the beginning of the files named "share*.r" In Java codes, these paths should be entered in identified lines at the beginning of the files named "AgentG.java" and "Registrar.java"

Model results and input datasets are available at [datasets repository placeholder]
