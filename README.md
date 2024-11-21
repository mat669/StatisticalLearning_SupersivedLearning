# Customer Satisfaction Analysis in the Airline Industry

This repository contains the code and analysis for a supervised learning project investigating drivers of customer satisfaction in the airline industry. Using a dataset of 130,000 observations, the project applies logistic regression, tree-based methods, and k-Nearest Neighbors (k-NN) to predict customer satisfaction and identify its key determinants.

## Project Structure

- **`MatteoCiarrocchi_SupervisedLearning_Code.R`**: R script implementing the supervised learning algorithms and analysis.
- **`MatteoCiarrocchi_SupervisedLearning_Report.pdf`**: Detailed report summarizing the problem, methodology, and key findings.

## Key Objectives

1. Investigate the drivers of customer satisfaction in the airline industry.
2. Predict customer satisfaction using logistic regression, tree classifiers, and k-NN algorithms.
3. Evaluate the importance of features such as inflight WiFi service, online boarding, and customer type.

## Dataset

The dataset, sourced from Kaggle, includes:
- **130,000 observations**.
- Variables such as customer type, age, flight distance, service ratings (e.g., WiFi, boarding), and satisfaction levels (satisfied, neutral, dissatisfied).

Key features:
- **Dependent Variable**: `satisfaction` (binary: satisfied or neutral/dissatisfied).
- **Independent Variables**: Gender, customer type, travel class, inflight service ratings, delays, etc.

## Methodology

### Logistic Regression
- Model fitted to predict customer satisfaction.
- Multicollinearity handled using stepwise feature selection and variance inflation factors (VIF).

### Tree-Based Methods
- Decision trees provide interpretable models for customer satisfaction.
- Pruned trees were used to optimize performance and reduce overfitting.

### k-Nearest Neighbors (k-NN)
- Used for classification with hyperparameter tuning for the number of neighbors (`k`).
- Achieved accuracy comparable to logistic regression.

## Results

1. Logistic regression achieved:
   - **Accuracy**: ~93%.
   - Identified key features like online boarding and inflight WiFi service.

2. Decision trees:
   - Provided interpretability but with slightly reduced accuracy (~85%).

3. k-NN:
   - Achieved comparable accuracy to logistic regression (~93% with `k=5`).

For a proper presetation, feel free to delve into **`supervised presentation.pdf`**!
