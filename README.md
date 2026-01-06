# Machine-learning-project
ML projects in credit ratings

## Project Workflow & Architecture

To achieve the final scaling system, the project followed a multi-stage data science pipeline. The code currently hosted in this repository focuses on **Phase 3: Scaling & Optimization**.

```mermaid
graph TD
    %% Phase 1
    subgraph Phase_1 [Phase 1: Data Discovery & Labeling]
    A[Data Collection] --> B[Feature Engineering]
    B --> C[K-Means Clustering/GMM]
    C --> D[Pseudo-labeling]
    end

    %% Phase 2
    subgraph Phase_2 [Phase 2: Predictive Modeling]
    D --> E[Random Forest Classifier]
    E --> F[Power Curve Prediction]
    end

    %% Phase 3
    subgraph Phase_3 [Phase 3: Analysis & Scaling - Current Repo]
    F --> G[Power Curve Analysis]
    G --> H[Polygonal Approximation]
    H --> I[Standardized Measurement Scale]
    end

    style Phase_3 fill:#f9f,stroke:#333,stroke-width:2px
