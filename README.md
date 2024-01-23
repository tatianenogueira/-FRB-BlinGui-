# FRB-BlinGui
Fuzzy Rule-Based Blind Navigation and Guidance mode (FRB-BlinGui) is an approach to improve a wearable device that detects obstacles for the visually impaired. FRB-BlinGui is a comprehensive solution consisting of three key phases: Learning, Risk Prediction, and Risk Alert Management. Here's a brief overview of each step:

1. Learning Phase
During the Learning Phase, the system leverages the RealDataset.csv and FuzzyRulesGeneration.R files to generate essential rules. These rules are crucial for embedding in the FuzzyEmbeddedSystem.zip, which serves as the source code for the Arduino device responsible for Risk Alert Management.

2. Risk Prediction
In this phase, FRB-BlinGui utilizes the learned rules to predict and assess potential risks based on the provided data.

3. Risk Alert Management
The Risk Alert Management phase involves the implementation of the FuzzyEmbeddedSystem.zip code on an Arduino device. This device is designed to efficiently manage and respond to identified risks, providing a comprehensive solution for risk mitigation.

For detailed instructions on how to use and implement each phase, refer to the published paper.
