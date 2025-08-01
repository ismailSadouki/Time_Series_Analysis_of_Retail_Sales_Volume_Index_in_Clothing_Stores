# 📈 Time Series Analysis of Retail Sales Volume Index in Clothing Stores

**Author**: Ismail Sadouki  
**Supervisor**: Prof. Smicha Aitamokhtar  
**Institution**: École Nationale Supérieure de Statistique et d'Économie Appliquée (ENSSEA)  
**Course**: Time Series Analysis  
**Period Covered**: February 1999 – April 2016  
**Data Source**: INSEE – Institut National de la Statistique et des Études Économiques  

# 📈 Time Series Analysis of Retail Sales Volume Index in Clothing Stores

**Author**: Ismail Sadouki  
**Supervisor**: Prof. Smicha Aitamokhtar  
**Institution**: École Nationale Supérieure de Statistique et d'Économie Appliquée (ENSSEA)  
**Course**: Time Series Analysis  
**Period Covered**: February 1999 – April 2016  
**Data Source**: INSEE – Institut National de la Statistique et des Études Économiques  

---

## 🔍 Project Overview

This project analyzes and forecasts the **Turnover Volume Index** of retail sales in clothing stores in France using time series analysis. The main goal is to understand sales behavior over time and make short-term forecasts using **ARIMA models** based on the **Box-Jenkins methodology**.

---
### 🧾 Abstract

This study explores the **Turnover Volume Index** for retail sales in clothing, focusing on specialized stores over a period of **17 years**, from **February 1999 to April 2016**. The data, sourced from **INSEE**, is **seasonally adjusted** and indexed to **2010** (base year = 100).

The analysis aims to assess trends and forecast future sales volumes using the **ARIMA (AutoRegressive Integrated Moving Average)** model. The **Box-Jenkins methodology** was employed to identify optimal model parameters through steps like stationarity testing, model identification, and diagnostic checking.

To ensure data stationarity, statistical tests such as the **Augmented Dickey-Fuller (ADF)** and **Phillips-Perron (PP)** tests were conducted. The findings indicate that despite short-term fluctuations, the clothing retail sales volume index generally centers around the base year value, with visible **seasonal variations**.

The study concludes by generating **forecasts** using the selected ARIMA model, offering data-driven insights into future trends in the retail clothing sector.


📘 Full PDF: See [`Time_Series_Analysis_of_Retail_Sales_Volume_Index_in_Clothing_Stores.pdf`](./Time_Series_Analysis_of_Retail_Sales_Volume_Index_in_Clothing_Stores.pdf)
![](https://i.imgur.com/Cr0db5y.png)

---

## 📊 Dataset Details

- **Frequency**: Monthly  
- **Observations**: 208  
- **Base Year**: 2010 (Index = 100)  
- **Sector**: Retail sale of clothing in specialized stores (NAF Rev. 2 - Class 47.71)  
- **Adjustments**: Seasonally adjusted (SA-WDA)

---

## 🧠 Methodology

### 🔧 Preprocessing
- Checked for stationarity using **ADF** and **Phillips-Perron** tests.
- Applied **first differencing** to remove trend.
- Performed **Levene’s Test** to verify homogeneity of variance.

### 📐 Model Selection
- Used **ACF/PACF** plots and **AIC/BIC** to identify candidate models:  
  - ARIMA(1,1,1)  
  - ARIMA(2,1,1)  
  - ARIMA(3,1,1)  
- Focused on **ARIMA(2,1,1)** and **ARIMA(1,1,1)** for their balance between performance and parsimony.

### 🧪 Diagnostics
- **Residual checks**: Ljung-Box tests on residuals and squared residuals (ARCH effect check)  
- **Normality checks**: Shapiro-Wilk and Jarque-Bera tests  
- **Outlier detection** using Z-scores and IQR, followed by imputation and model re-fitting.

---

## 📈 Forecasting

- Performed **24-step ahead** forecasting using both ARIMA(2,1,1) and ARIMA(1,1,1).
- Compared performance using:
  - RMSE
  - MAE
  - MAPE
  - ME
- **Hold-out evaluation** using test data from May 2015 to April 2016.

---

## ✅ Key Findings

- The **ARIMA(1,1,1)** model provided slightly better out-of-sample performance.
- Residuals of both models became normally distributed after outlier imputation.
- Both models effectively captured time dependence, but ARIMA(1,1,1) was preferred for its **simplicity and robustness**.

---

## ⚠️ Limitations & Future Work

- The model is **univariate** and doesn't consider **external factors** (e.g., economic indicators, holidays).
- Future enhancements could include:
  - **ARIMAX models** with exogenous variables.
  - Rolling forecast evaluation for model stability.
  - Comparison with **machine learning-based forecasting** models.

---

## 📚 References

1. Pankratz, A. _Forecasting with Univariate Box-Jenkins Models_.  
2. Hyndman, R. & Athanasopoulos, G. _Forecasting: Principles and Practice (3rd ed.)_.  
3. Various studies on ADF, ARIMA, and time series modeling in economic forecasting.

---

📘 PDF Report: See [`Time_Series_Analysis_of_Retail_Sales_Volume_Index_in_Clothing_Stores.pdf`](./Time_Series_Analysis_of_Retail_Sales_Volume_Index_in_Clothing_Stores.pdf)
