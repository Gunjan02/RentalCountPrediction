
import requests

url = "http://localhost:5000/predict_api"
r = requests.post(url,json = {'season':2,'year':1,'month':5,'weekday':1,'workingday':1,
                              'weathersit':3,'temperature':0.55,'humidity':0.8,'windspeed':0.3})
print(r.json())