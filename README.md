# Credit Card Fraud Detection

This project analyses credit card transactions dataset to predict whether the transaction is a fraud transaction or not. Using various predictive analysis models, we aim to identify the patterns in transactions that correlate with fraudulent status. This analysis includes classification methods, which enable us to understand the underlying structure in the data and access model effectiveness.
The models used are:
1.	K-Nearest Neighbors (K-NN): It is a supervised machine learning method employed to tackle classification and regression problems. The K-NN algorithm works by finding the K nearest neighbors to a given data point based on a distance metric, such as Euclidean distance. The class or value of the data point is then determined by the majority vote or average of the K neighbors. This approach allows the algorithm to adapt to different patterns and make predictions based on the local structure of the data.
   ![Screenshot 2024-11-26 091345](https://github.com/user-attachments/assets/b0b5b2dd-27e6-4013-aa66-a2e9a89f0452)

2.	Naïve Bayes Classifiers: It is used for classification problems. It is highly used in text classification. In text classification tasks, data contains high dimension (as each word represent one feature in the data). It is used in spam filtering, sentiment detection, rating classification etc. The advantage of using naïve Bayes is its speed. It is fast and making prediction is easy with high dimension of data.

    ![Screenshot 2024-11-26 091955](https://github.com/user-attachments/assets/30f4fe4e-ce6a-419a-ba46-b39db681f156)

3.	Decision Tree: It is a flowchart-like structure used to make decisions or predictions. It consists of nodes representing decisions or tests on attributes, branches representing the outcome of these decisions, and leaf nodes representing final outcomes or predictions. Each internal node corresponds to a test on an attribute, each branch corresponds to the result of the test, and each leaf node corresponds to a class label or a continuous value.

   ![decision_tree](https://github.com/user-attachments/assets/b3341c1f-abfc-4781-be0e-85fb4f31672a)

   ![Screenshot 2024-11-26 093036](https://github.com/user-attachments/assets/55149612-1641-4b83-88f8-f336c1bd406f)

4.	Support Vector Machine (SVM): It is a powerful machine learning algorithm widely used for both linear and nonlinear classification, as well as regression and outlier detection tasks. SVMs are highly adaptable, making them suitable for various applications such as text classification, image classification, spam detection, handwriting identification, gene expression analysis, face detection, and anomaly detection.
Each model is evaluated on its prediction accuracy, and results are visualized using confusion matrices to illustrate the model’s performance. A bar plot was plotted to make a comparison between the accuracies of all the models applied on the dataset, providing insights into which models best detect fraudulent transactions. This analysis can support financial security strategies, enabling proactive measures to prevent credit card fraud and protect consumers.
  ![Screenshot 2024-11-26 093504](https://github.com/user-attachments/assets/c038c91c-1a14-41b0-9af5-7b8df2a1045f)

