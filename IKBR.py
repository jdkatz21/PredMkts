import kalshi_python
import pprint

from KalshiClientsBaseV2 import ExchangeClient
import time
import json
import uuid

prod_email = "jareddeankatz@gmail.com" # change these to be your personal credentials
prod_password = "Zasx,12321,xsaZ" # (for extra security, we recommend using a config file)

# for prod

prod_api_base = "https://api.elections.kalshi.com/trade-api/v2"

## if wanting to test in prod:
exchange_client = ExchangeClient(exchange_api_base=prod_api_base, email = prod_email, password = prod_password)

# first we will check on the exchange status to confirm you are properly connected...
print(exchange_client.get_exchange_status())




# To start off, you need to have created an account at https://kalshi.com (Production) 
# or an account on the Demo https://demo.kalshi.co/

prod_email = "jareddeankatz@gmail.com" # change these to be your personal credentials
prod_password = "Zasx,12321,xsaZ" # (for extra security, we recommend using a config file)

demo_email = 'jareddeankatz@gmail.com' # change these to be syour personal credentials
demo_password = 'Zasx,12321,xsaZ' # (for extra security, we recommend using a config file)

# for prod
prod_api_base = "https://api.elections.kalshi.com/trade-api/v2"

# for demo
demo_api_base = "https://demo-api.kalshi.co/trade-api/v2"

## if wanting to test in prod:
exchange_client = ExchangeClient(exchange_api_base=prod_api_base, email = prod_email, password = prod_password)

## if wanting to test in demo
# exchange_client = ExchangeClient(exchange_api_base = demo_api_base, email = demo_email, password = demo_password)

# first we will check on the exchange status to confirm you are properly connected...
print(exchange_client.get_exchange_status())



# You can discover markets through the get_markets endpoint...

# and use query parameters to filter your search!
market_params = {'limit':100,
                    'cursor':None, # passing in the cursor from the previous get_markets call
                    'event_ticker': None,
                    'series_ticker':None,
                    'max_close_ts':None, # pass in unix_ts
                    'min_close_ts':None, # pass in unix_ts
                    'status':None,
                    'tickers':None}

markets_response = exchange_client.get_markets(**market_params)
cursor = markets_response['cursor']

print('keys:', markets_response.keys())
print()
print('number of objects:', len(markets_response['markets'])) # 100 objects!
print()
print('first market in payload:', markets_response['markets'][0])
print()
print('cursor:', cursor)



# What are cursors and how do they work?
    
# The Cursor represents a pointer to the next page of records in the pagination.
# So this optional parameter, when filled, should be filled with the cursor string returned in a previous request to this end-point.
# Filling this would basically tell the api to get the next page containing the number of records passed on the limit parameter.
# On the other side not filling it tells the api you want to get the first page for another query.
# The cursor does not store any filters, so if any filter parameters like tickers, max_ts or min_ts were passed in the original query they must be passed again.

# Let's try it in action! Suppose we wanted to get the next 100 market objects...

market_params = {'limit':100,
                    'cursor':cursor, # passing in the cursor from the previous get_markets call
                    'event_ticker': None,
                    'series_ticker': None,
                    'max_close_ts': None, # pass in unix_ts
                    'min_close_ts': None, # pass in unix_ts
                    'status': None,
                    'tickers':None}

markets_response = exchange_client.get_markets(**market_params)
cursor = markets_response['cursor']

print('keys:', markets_response.keys())
print()
print('number of objects:', len(markets_response['markets'])) # 100 objects!
print()
print('first market in market_response payload:', markets_response['markets'][0]) # new markets!
print()
print('new cursor!', cursor)


# Next, let's look at event level data by passing an event ticker to the get_event endpoint...

event_ticker = markets_response['markets'][5]['event_ticker']
event_params = {'event_ticker': event_ticker}
event_response = exchange_client.get_event(**event_params)

print('keys:', event_response.keys())
print()
print('event object:', event_response['event'])
print()
print('first market in event_response payload:', event_response['markets'][0])


# Next, let's look at series level data by passing a series ticker to the get_series endpoint! 
series_ticker = event_response['event']['series_ticker']
series_params = {'series_ticker': series_ticker}
series_response = exchange_client.get_series(**series_params)

print('keys:', series_response.keys())
print()
print('series object:', series_response['series'])
print()


# Next let's look at the recent market history for a market
ticker = 'FED-25DEC-T4.00'

market_history_params = {'ticker': ticker,
                            'limit': 100
                            }

market_info = exchange_client.get_market(ticker)


try:
    market_history_response = exchange_client.get_market_history(**market_history_params)
    print(market_history_response)
except Exception as e:
    print(f"Failed to fetch market history for {ticker}: {e}")



market_history_params = {'ticker': ticker,
                         'depth': 30}

orderbook_response = exchange_client.get_orderbook(**market_history_params)

print(orderbook_response.keys())


# This works!!!
trades = exchange_client.get_trades(ticker='FED-24JAN-T5.50')



######### USING REQUESTS

url = "https://demo-api.kalshi.co/trade-api/v2/markets/FED-24JUN-T4.25"

headers = {"accept": "application/json"}

response = requests.get(url, headers=headers)

print(response.text)


url = "https://demo-api.kalshi.co/trade-api/v2/series/FED"

headers = {"accept": "application/json"}

response = requests.get(url, headers=headers)

print(response.text)

end_ts = int(time.time())
start_ts = end_ts - 100000  # last 1 hour
# Query parameters
params = {
    "start_ts": start_ts,
    "end_ts": end_ts,
    "period_interval": "1m"  # 5-minute candles
}


url = "https://demo-api.kalshi.co/trade-api/v2/series/FED/markets/FED-25JUN-T4.25/candlesticks"

headers = {"accept": "application/json"}

response = requests.get(url, headers=headers, params=params)

print(response.text)


# CHat gpt version


# Replace with a ticker that has volume
series = "INFL"
ticker = "INFL-24JUN-CPI-3.4"

end_ts = int(time.time())
start_ts = end_ts - 86400  # last 24 hours

url = f"https://demo-api.kalshi.co/trade-api/v2/series/{series}/markets/{ticker}/candlesticks"

params = {
    "start_ts": start_ts,
    "end_ts": end_ts,
    "period_interval": "5m"
}

response = requests.get(url, headers={"accept": "application/json"}, params=params)

print(response.status_code)
print(response.text)


















