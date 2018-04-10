library(readxl)
library(zoo)
#군단위 인구소멸 데이터 
woman <- read.csv('C:/Users/DJA/Desktop/DataScience/teamproject/woman_monthly_b20.csv')
senior <- read.csv('C:/Users/DJA/Desktop/DataScience/teamproject/senior_monthly_b20.csv')
a<- read_excel('C:/Users/DJA/Desktop/DataScience/rateonetoone.xlsx')
top <- read_excel('C:/Users/DJA/Desktop/DataScience/Best20_rate.xlsx')

plot(woman$YYMM, woman$전라남도.고흥군, type='l')
plot(woman$YYMM, woman$전라남도.고흥군, type='l')
dates <- as.yearmon(2008 + seq(0,119)/12)
dates
str(dates)

tmp <- woman$전라남도.고흥군
tmp

#1) 전라남도 고흥군 데이터를 Time Series로 변환 작업.
t_tmp <- ts(tmp)
plot(t_tmp,start=c(2008),xaxt = "n")

tsp = attributes(t_tmp)$tsp
dates = seq(as.Date("2008-01-01"), by = "month", along = t_tmp); dates;
axis(1, at = seq(tsp[1], tsp[2], along = t_tmp), labels = format(dates, "%Y"))
t_tmp

#2)이동평균법 또는 차분으로 그래프의 패턴을 보이기 위해 부드럽게 만드는 작업
#이동평균법
library(TTR)
ksma3 <- SMA(t_tmp, n = 3) #3개월간의 이동 평균치
ksma8 <- SMA(t_tmp, n = 8) #8개월간의 이동 평균치
ksma12 <- SMA(t_tmp, n = 12) #12개월간의 이동 평균치

par(mfrow = c(2,2))

plot.ts(t_tmp)
plot.ts(ksma3)
plot.ts(ksma8)
plot.ts(ksma12)

#차분
kdiff1 <- diff(t_tmp, differences = 1)
kdiff2 <- diff(t_tmp, differences = 2)
kdiff3 <- diff(t_tmp, differences = 3)
kdiff4 <- diff(t_tmp, differences = 4)
kdiff5 <- diff(t_tmp, differences = 5)

plot.ts(t_tmp)
plot.ts(kdiff1)    # 1차 차분만 해도 어느정도 정상화 패턴을 보임
plot.ts(kdiff2)
plot.ts(kdiff3)
plot.ts(kdiff4)
plot.ts(kdiff5)

#차트를 원상태로 바꿈
par(mfrow = c(1,1))
mean(kdiff2); sd(kdiff2)

#arima 매개변수 조사
# lag 6에서 절단값 --> AR(3)
pacf(kdiff2, lag.max = 20)  

# 1차 차분한 데이터로 ARIMA 모형 확인
# lag 2부터 점선 안에 존재. 
#lag 절단값 = 3. --> MA(1)
acf(kdiff2, lag.max = 20)   

#결론 : --> ARIMA(2,6,1)

#box-pierce검증
Box.test(t_tmp, type='Ljung-Box') 
Box.test

# 자동으로 ARIMA 모형 확인
# 최적값 --> ARIMA(0,1,1)
library(forecast)
auto.arima(t_tmp)

# 예측
karima <- arima(t_tmp, order = c(0,2,1))    
# 차분통해 확인한 값 적용
karima

#예측값 측정
kfcast <- forecast(karima, h = 120)
kfcast
plot(forecast(kfcast))

