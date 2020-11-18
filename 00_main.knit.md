---
output: 
  pdf_document:
    toc: no
    toc_depth: 4
bibliography: add/doc.bib
linkcolor: red
csl: add/ieee.csl
header-includes:
- \usepackage{pdfpages}
- \usepackage{amsmath}
---



\pagenumbering{gobble}

```{=tex}
\includepdf{add/titelblatt.pdf}
\includepdf{add/Erklaerung_BA.pdf}
```
\tableofcontents

\newpage





## Abstract

\newpage

\pagenumbering{arabic}





## 1. Introduction

The main purpose of trading is buying and selling stocks, bonds, or other financial instruments with increasing the returns of the investments in mind while maintaining relatively low risk. With the help of a trading strategy, an investor can try to improve his performance. One can simply divide the strategies into passive and active. The praised and well established passive strategy buy-and-hold takes no short price movements into account. Positioning and trading based on these short price movements are considered active trading.

This paper applies time-series analysis to these short price movements to create active trading strategies. The objective of these developed strategies is to outperform the buy-and-hold strategy.

### 1.1. The data used in this paper

The dataset which will be analyzed in this paper contains 4 tradeable indexes, a visualization of the data is shown below in figure \ref{fig:chap1.1}.

\begin{figure}

{\centering \includegraphics[width=0.7\linewidth]{00_main_files/figure-latex/chap1.1-1} 

}

\caption{Visualization of the 4 indexes}\label{fig:chap1.1}
\end{figure}

Each time-series has 4306 observations and starts from October 2003 to April 2020. In all indexes is an upward drift observable, during the time period of the great recession (2008) is a slight bump visible. Also later in 2013 and 2016 are small break-ins evident. More interesting is the up and down behavior at the end of the series during the Covid19 pandemic.

\newpage

In addition, to the indexes, the dataset contains 8 different interest rates of treasury bonds which will be used for further analysis. A few key-values of the interest rates are shown in the following table \ref{tab:inttable}.

\begin{table}[!h]

\caption{\label{tab:inttable}Summary of the 8 interest rates.}
\centering
\begin{tabular}[t]{llrrrr}
\toprule
  & Maturity & Mean & Volatility & Min. & Max.\\
\midrule
\cellcolor{gray!6}{Interest 1} & \cellcolor{gray!6}{3M} & \cellcolor{gray!6}{4.09} & \cellcolor{gray!6}{4.95} & \cellcolor{gray!6}{-0.28} & \cellcolor{gray!6}{16.27}\\
Interest 2 & 6M & 4.47 & 5.05 & 0.01 & 16.73\\
\cellcolor{gray!6}{Interest 3} & \cellcolor{gray!6}{1Y} & \cellcolor{gray!6}{4.64} & \cellcolor{gray!6}{4.22} & \cellcolor{gray!6}{0.20} & \cellcolor{gray!6}{10.51}\\
Interest 4 & 2Y & 5.42 & 4.58 & 0.49 & 16.58\\
\cellcolor{gray!6}{Interest 5} & \cellcolor{gray!6}{3Y} & \cellcolor{gray!6}{6.16} & \cellcolor{gray!6}{4.33} & \cellcolor{gray!6}{0.75} & \cellcolor{gray!6}{16.51}\\
\addlinespace
Interest 6 & 5Y & 7.41 & 3.82 & 1.06 & 16.44\\
\cellcolor{gray!6}{Interest 7} & \cellcolor{gray!6}{7Y} & \cellcolor{gray!6}{9.50} & \cellcolor{gray!6}{3.31} & \cellcolor{gray!6}{1.71} & \cellcolor{gray!6}{16.63}\\
Interest 8 & 10Y & 11.61 & 2.93 & 3.13 & 17.47\\
\bottomrule
\end{tabular}
\end{table}
A typical characteristic of interest rates is shown in the given data. A bond with longer maturities is often associated with higher returns compared with those with shorter maturities. An investor which invests in short-term treasury bonds will have his gain earlier but will be confronted with a lower return.

A more in depth analysis of the given dataset will follow in section [3.1](#ts-analysis).

### 1.2. Ojective of this paper

The objective of this paper is to trade these 4 indexes with an active trading strategy. The main objective is to outperform the passive buy-and-hold strategy. Methods such as the Moving-Average-Filter or the ARMA-GARCH-Model provide signals for either long or short the position to maximize the return of the investments in these indexes.

The performance of these strategies are build open various different parameters and conditions. The lengths of the filters applied to a Moving-Average may result in different solutions. Models could perform differently for any given length of the in-sample or out-of-sample scope. The necessity of including a historical crisis in the starting-sample can decide if a model performs better or worse than another. The correct validation of model parameters could have a significant impact on the forecasts.

In addition to all criteria and conditions, the strategies can be further adjusted by composing different weighted portfolios. Estimated predicted volatility can be used to modulate the position size to mitigate the risk.

Challenging will be finding the most optimal model in this wide field of conditions and parameters. The buy-and-hold strategy will be used as a benchmark to be compared with the developed active trading strategies. Computing and comparing the Sharpe ratios of each model can serve as an indicator to rely on for better or worse models.

\newpage



## 2. Theory

It is assumed that the reader of this paper already has basic knowledge of the mathematical principles of time series analysis. Therefore, this section will only briefly describe the mathematical models and processes.

### 2.1. Time-Series

Almost anything with a data point to a given timestamp can be named a time-series. The monthly gross domestic product, the weekly US gasoline production, or daily stock price movements. In this paper lies the focus of the analysis of financial time series. Due to trades often only take place during the week, there are gaps in the time series on the weekends, an exception would be the trading of cryptocurrencies like Bitcoin which are also tradeable at the weekends.

A series of data points with more or less equidistant time points $t$ with the sample length of $T$, is called a time-series $x_{t}, t=1,...,T$ [@eco1]. The analysis of a time-series $x_{t}$ involves creating a reasonable model that can be utilized to perform forecast predictions.

#### 2.1.1. Stationarity {#stationarity}

&nbsp;

In order to fit a suitable model with a given time series $x_{t}$, the assumptions of stationarity must be met. In this practical application, only the following weak-stationarity properties are required.

\begin{equation}
  \label{eq:mean}
  E[x_{t}]=\mu
\end{equation}

\begin{equation}
  \label{eq:var}
  Var(x_{t})=\sigma_{x}^{2}
\end{equation}

\begin{equation}
  \label{eq:cov}
  Cov(x_{t}, x_{t-k})=R(k)
\end{equation}

Many financial time-series are subject to shift, trends or changing volatility. In figure \ref{fig:chap2.1} are the stock prices of Alphabet Inc Class A (Google) visualized. This time-series shows a clear upwards drift and towards the end the volatility increases.

\begin{figure}

{\centering \includegraphics[width=0.7\linewidth]{00_main_files/figure-latex/chap2.1-1} 

}

\caption{Visualization of the adjusted prices of the Alphabet Inc Class A Stock.}\label{fig:chap2.1}
\end{figure}
\newpage

To improve the violated properties the first difference can be applied and additionally a logarithmic transformation can be performed [@slide_eco3_1]. The log-returns transformation can only performed to strict positive data.

$$\mathrm{LogReturn} = \mathrm{log}(x_{t})-\mathrm{log}(x_{t-1})$$

The result is the so-called log-returns.

\begin{figure}

{\centering \includegraphics[width=0.7\linewidth]{00_main_files/figure-latex/chap2.2-1} 

}

\caption{Visualization of the Log-Returns}\label{fig:chap2.2}
\end{figure}
Applying the transformation to the data causes the drift to disappear, but the series still contains stronger and weaker volatile phases. This effect often occurs in non-stationary financial data and is called volatility cluster. This special property is used for the modelling of forecast models, which will be discussed in chapter [2.2](#models-section).

#### 2.1.2. Autocorrelation

&nbsp;

The autocorrelation function (ACF) reveals how the correlation between any two data points of the time series changes as their separation changes [@acf]. More precisely, acf measures the dependence between $x_{t}$ and $x_{t \pm k}$ at lag $k$. The partial autocorrelation (PACF) measures the dependency between $x_{t}$ and $x_{t-k}$ at lag $k$ [@eco1]. For stationary time series, ACF can be used to identify the model order of a MA-process, PACF for AR-processes.

In the following figure \ref{fig:chap2.1.2} are ACF and PACF of the non-stationary adjusted Google stock visualized. Both graphics show the typical pattern of a non-stationary time series. The plot above shows the dependence structure of the time series. This means that it takes a long time until the series changes. Often a large value is followed by another large value, which indicates a strong trend. This property of the series can be seen in figure \ref{fig:chap2.1} as the long upward drift. The plot below indicates a significant partial autocorrelation at lag $k=1$.

In the following section [2.2.](#models-section) the characteristics of the autocorrelation function can be used for the verification of ARIMA and ARCH-processes.

\begin{figure}

{\centering \includegraphics[width=0.7\linewidth]{00_main_files/figure-latex/chap2.1.2-1} 

}

\caption{Acf and Pacf of the Adjusted Prizes of Google.}\label{fig:chap2.1.2}
\end{figure}

\newpage

### 2.2. Models {#models-section}

The following processes are used to determine certain properties and characteristics of a time series so that they are transformed into a model. The goal is to fit the time series as well as possible in order to create reliable forecasts.

#### 2.2.1. ARIMA

&nbsp;

An ARIMA(*p*,*d*,*q*) process is defined as follows.

\begin{equation} \label{eq:arima}
  x_{t}=c+a_{1}x_{t-1}+...+a_{p}x_{t-p}+\epsilon_{t}+b_{1}\epsilon_{t-1}+...+b_{q}\epsilon_{t-q}
\end{equation}

- $p$ and $q$ are the AR- and MA-model orders
- $a$ and $b$ are the AR- and MA-model parameters
- $d$ is the differential parameter
- $\epsilon_t$ is a white noise sequence
- $x_t$ is the given data $x_{1},...,x_{T}$

The mean of an ARIMA-process can be computed as: 

$$\mu=\frac{c}{1-a_{1}-...-a_{p}}$$

ARIMA processes can be divided into 4 different models. Choosing a model that best represents the time series is a difficult task. The goal is to find the best possible model with as few parameters as possible.

The previously introduced ACF and PACF can help to determine the orders of simple models. Provided that the time series is stationary, the model orders can be determined directly. For an AR(*p*)-process (ARIMA(*p*,*0*,*0*)), the ACF plot will gradually decrease and simultaneously the PACF should have a sharp drop after *p* significant lags. For an MA(*q*)-process (ARIMA(*0*,*0*.*q*)) the opposite is true, the ACF should show a sharp drop after a certain *q* number of lags while PACF should show a gradual decreasing trend. If both ACF and PACF show a gradual decreasing pattern, then the ARIMA(*p*,*0*,*q*)-process should be considered for modeling [@arima]. If the time series is not stationary, differentiation can be considered (ARIMA(*p*,*d*,*q*)).

\newpage

The application of an analysis method to Google prices finds an ARIMA(*1*,*1*,*0*) as the optimal model. This makes sense if you look back at figure \ref{fig:chap2.1.2}. Long dependency structures in the ACF plot indicating an AR(*p*) process and at the same time after lag=$1$=*p* the PACF has a strong drop. The differential operator *d*=$1$ transforms the non-stationary series into a stationary one.

\begin{figure}

{\centering \includegraphics[width=0.7\linewidth]{00_main_files/figure-latex/chap2.2.1-1} 

}

\caption{ARIMA-Forecast}\label{fig:chap2.2.1}
\end{figure}


#### 2.2.2. ARCH & GARCH {#arch-garch-section}

&nbsp;

The volatility clustering mentioned in section [2.1.1](#stationarity) can be handled with an auto-regressive conditional heteroscedastic process.

\begin{align} \label{eq:arch}
  \epsilon_{t} &= \mathrm{log}(x_{t})-\mathrm{log}(x_{t-1}) \nonumber \\
  \epsilon_{t} &= \sigma_{t}u_{t} \\
  \sigma_{t}^{2} &=c \sigma^{2}+\sum_{k=1}^{m}\beta_{k}\epsilon_{t-k}^{2} \nonumber
\end{align}

with:

- $x_{t}$ is the original data (often non-stationary)
- $\epsilon_{t}$ is the stationary log-return
- $u_{t}$ is independent and identically distributed (iid) and standardized random variable
- $\sigma^{2}$ is the unconditional variance of the process $\epsilon_{t}$.
- $\sigma_{t}^{2}$ is the conditional variance of the process $\epsilon_{t}$.

The ARCH-process can be generalized by adding the lagged conditional variances to the equation \ref{eq:arch}.

\begin{align} \label{eq:garch}
  \epsilon_{t} &= \mathrm{log}(x_{t})-\mathrm{log}(x_{t-1}) \nonumber \\
  \epsilon_{t} &= \sigma_{t}u_{t} \\
  \sigma_{t}^{2} &=c \sigma^{2}+\sum_{j=1}^{n}\alpha_{j}\sigma_{t-j}^{2}+\sum_{k=1}^{m}\beta_{k}\epsilon_{t-k}^{2} \nonumber
\end{align}

#### 2.2.3. ARIMA-GARCH

&nbsp;

Another process is the combination of ARIMA and GARCH processes.

\begin{eqnarray}
y_{t}&=&\mu + a_{1}y_{t-1}+...+a_{p}y_{t-p}+\epsilon_{t}+b_{1}\epsilon_{t-1}+...+b_{q}\epsilon_{t-q} \label{eq:arima-garch1} \\
\epsilon_{t}&=&\sigma_{t}u_{t} \nonumber \\
\sigma_{t}^{2}&=&c\sigma^{2}+\sum_{j=1}^{n}\alpha_{j}\sigma_{t-j}^{2}+\sum_{k=1}^{m}\beta_{k}\epsilon_{t-k}^{2} \label{eq:arima-garch2}
\end{eqnarray}

Is called the mean-equation \ref{eq:arima-garch1}

Is called the variance-equation \ref{eq:arima-garch2}

[@eco2]

\newpage

### 2.3. Moving Average Filters

moving average filters are basically used to identify trends and smooth out price fluctuations. As a commonly used tool moving average filters are very simple in its usage, historical data was summarized and divided bi the length of the filter. The actual challenge in using Moving average filters is to figure out which length of the filter brings the most useful information. 


#### 2.3.1. Equally-weighted Moving Average

&nbsp;

SMA stands for Simple Moving Average which. depending on the length of the filter(l) l obervations since the last noted observation will be considered. theyre getting summarized and divided by the filterlength equals the EqMA. For every timestep a new observation is considered and the last one eliminated.

EqMA

\begin{equation}
  \label{eq:eqma}
  y_{t}=\frac{1}{L}\sum_{k=0}^{L-1}x_{t-k}
\end{equation}

- ${L}$ = filterlength

#### 2.3.2. Exponentially-weighted Moving Average

&nbsp;

Since not all observations are having the same influence of future value we can apply a weight to past observations. One method will be exponentially weighted Moving average. So we chose an optimal parameter to give past observations weights decreasing by alpha

A skillfull trader chose an optimal to increase the performace of the measurement. Weights could also be given individually by adding a weight vector to the filter.

EMA

\begin{equation}
  \label{eq:ema}
  y_{t}=\frac{1}{\sum_{k=0}^{m}\alpha^{k}}\sum_{k=0}^{m}\alpha^{k}x_{t-k}
\end{equation}

- ${m}$    = filterlength
- ${\alpha}$ = Parameter to weigh the observations


#### 2.3.3. Moving Average Crossings

&nbsp;

\begin{equation}
  \label{eq:mac}
  y_{t}=\frac{1}{L_1}\sum_{k=0}^{L_1-1}x_{t-k} - \frac{1}{L_2}\sum_{k=0}^{L_2-1}x_{t-k}
\end{equation}

- ${L_1}$    = filterlength 1
- ${L_2}$    = filterlength 2
- ${0 < \alpha < 1}$  = Parameter to weigh the observations 

Moving average crossings are basically just different MA's with different lengths applied to a time-series. The points the filters then cross will be used as a trading signal to go long, short or hold.

An easy example of Ma average crossings with 2 mas of different length is visualized in # \ref{fig:chap2.1}.



\begin{center}\includegraphics[width=0.7\linewidth]{00_main_files/figure-latex/chap2.3.3 -1} \end{center}



\newpage

### 2.4. Real Strength Index

### 2.5. Sharpe Ratio

&nbsp;
 
Sharpe ratio is a very powerful and widely used ratio to measure performance. It describes return per risk 

\begin{equation}
  \label{eq:Sharperatio}
  \mathrm{SharpeRatio}=\frac{R_{p}-R_{f}}{\sigma}
\end{equation}

- ${R_p}$ = Return of Portfolio
- ${R_p}$ = Risk free Rate, mostly treasury bonds
- ${\sigma_p}$ = standard deviation of portfolios excess return (risk)

### 2.6. Carry

carry trades are trading strategies where usually money is borrowed at a lower intrest rate than the investment is gioving in return.
the risk of this strategy is based in the currency risk.


### 2.7. Value

### 2.8  Bollinger bands

&nbsp;

Bollinger bands are a analysis tool founded by John Bollinger. It contains a moving average and an upper and lower band . The bands are defined by adding  a constant${K}$ times a standard deviation ${\sigma_t}$ to the *Moving Average* for the upper , and subtracting it for the lower band.

\begin{equation}
  \label{eq:lower and upper bollingerbands}
  \mathrm{U_t}=MA_{t}+K\sigma, L_{t}=MA_{t}-K\sigma
\end{equation}

the variance from bollingers theory is calculated by:

\begin{equation}
  \label{eq:variance bollingerbands}
  \mathrm{\sigma_{t}^2}=\frac{1}{N}\sum_{k=0}^{N-1}(x_{t-k} -MA_t)^2
\end{equation}

the calculated ${\sigma_t}$ could be problematic because its derived from the original series and increases with the level. Therefore an other method to calculate the standart deviation could be used.


- ${N}$ = usually the filterlength and the lentgh considered for ${\sigma}$ the same
- ${K}$ = Constant usually equals 2
- $\sigma_p$ = standard deviation of the series
- ${U_t}$ = upper band
- ${L_t}$ = lower band



\begin{center}\includegraphics[width=0.7\linewidth]{00_main_files/figure-latex/chap2.8 -1} \end{center}



\newpage



## 3. Methodology

In this section models were created trying to outperform the buy and hold strategy.
starting with the usage of the simplest models , diffrent aproaches were chosen to fullfill the goal.



### 3.1. Time-Series Analysis {#ts-analysis}

Plots of the timeseries, decomposition. Stationarity (refer to the theory section)

### 3.2 using simple methods


\begin{figure}

{\centering \includegraphics[width=0.7\linewidth]{00_main_files/figure-latex/chap3.2-1} 

}

\caption{conversion data}\label{fig:chap3.2}
\end{figure}

\newpage



## 4. Conclusion

\newpage



## 5. References

<div id="refs"></div>

\newpage



## Attachment
