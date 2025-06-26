# Attempts to auth onto Kashi prod env

import os
from dotenv import load_dotenv
from cryptography.hazmat.primitives import serialization
import asyncio

from clients2 import KalshiHttpClient, KalshiWebSocketClient, Environment

# Load environment variables
load_dotenv('env.env')
env = Environment.PROD # toggle environment here
KEYID = os.getenv('DEMO_KEYID') if env == Environment.DEMO else os.getenv('KALSHI_KEYID')
KEYFILE = os.getenv('DEMO_KEYFILE') if env == Environment.DEMO else os.getenv('KALSHI_KEYFILE')

try:
    with open(KEYFILE, "rb") as key_file:
        print(key_file)
        private_key = serialization.load_pem_private_key(
            key_file.read(),
            password=None  # Provide the password if your key is encrypted
        )
except FileNotFoundError:
    raise FileNotFoundError(f"Private key file not found at {KEYFILE}")
except Exception as e:
    raise Exception(f"Error loading private key: {str(e)}")

# Initialize the HTTP client
client = KalshiHttpClient(
    key_id=KEYID,
    private_key=private_key,
    environment=env
)



# This file exists to pull all trade data surrounding federal funds rate 
# prediction markets from Kalshi


# You'll need the KashiClientsBaseV2.py file
from KalshiClientsBaseV2 import ExchangeClient
import time
import json
import uuid
import pandas as pd
import datetime
import kalshi_python


# Get a list of the tickers we want
tickers = [
    
    'FED-22JAN-T0.25',
    'FED-22MAR-T0.25', 'FED-22MAR-T0.5', 'FED-22MAR-T0.75',
    'FED-22MAY-T0.25', 'FED-22MAY-T0.5', 'FED-22MAY-T0.75', 'FED-22MAY-T1', 
    'FED-22JUN-T0.25', 'FED-22JUN-T0.5', 'FED-22JUN-T0.75', 'FED-22JUN-T1', 'FED-22JUN-T1.25', 'FED-22JUN-T1.50', 'FED-22JUN-T1.75', 
    'FED-22JUL-T0.25', 'FED-22JUL-T0.5', 'FED-22JUL-T0.75', 'FED-22JUL-T1', 'FED-22JUL-T1.25', 'FED-22JUL-T1.50', 'FED-22JUL-T1.75', 'FED-22JUL-T2.00', 'FED-22JULY-T2.25', 'FED-22JULY-T2.50', 'FED-22JUL-T2.75', 'FED-22JUL-T3.00', 'FED-22JUL-T3.25', 'FED-22JUL-T3.50', 'FED-22JUL-T2.75',
    'FED-22SEP-T0.25', 'FED-22SEP-T0.5', 'FED-22SEP-T0.75', 'FED-22SEP-T1', 'FED-22SEP-T1.25', 'FED-22SEP-T1.5', 'FED-22SEP-T1.75', 'FED-22SEP-T2.00', 'FED-22SEP-T2.25', 'FED-22SEP-T2.50', 'FED-22SEP-T2.75', 'FED-22SEP-T3.00', 'FED-22SEP-T3.25', 'FED-22SEP-T3.50', 
    'FED-22NOV-T0.25', 'FED-22NOV-T0.5', 'FED-22NOV-T0.75', 'FED-22NOV-T1', 'FED-22NOV-T1.25', 'FED-22NOV-T1.5', 'FED-22NOV-T1.75', 'FED-22NOV-T2.00', 'FED-22NOV-T2.25', 'FED-22NOV-T2.50', 'FED-22NOV-T2.75', 'FED-22NOV-T3.00', 'FED-22NOV-T3.25', 'FED-22NOV-T3.50', 'FED-22NOV-T2.75', 'FED-22NOV-T3.00', 'FED-22NOV-T3.25', 'FED-22NOV-T3.50', 'FED-22NOV-T3.75', 'FED-22NOV-T4.00',
    'FED-22DEC-T0.25', 'FED-22DEC-T0.5', 'FED-22DEC-T0.75', 'FED-22DEC-T1', 'FED-22DEC-T1.25', 'FED-22DEC-T1.5', 'FED-22DEC-T1.75', 'FED-22DEC-T2.0', 'FED-22DEC-T2.25', 'FED-22DEC-T2.50', 'FED-22DEC-T2.75', 'FED-22DEC-T3.00', 'FED-22DEC-T3.25', 'FED-22DEC-T3.50', 'FED-22DEC-T2.75', 'FED-22DEC-T3.00', 'FED-22DEC-T3.25', 'FED-22DEC-T3.50', 'FED-22DEC-T3.75', 'FED-22DEC-T4.00', 'FED-22DEC-T4.25', 'FED-22DEC-T4.50', 'FED-22DEC-T4.75', 'FED-22DEC-T5.00', 'FED-22DEC-T5.25', 'FED-22DEC-T5.50', 'FED-22DEC-T5.75', 'FED-22DEC-T6.00',  
    
    'FED-23FEB-T1.75', 'FED-23FEB-T2.00', 'FED-23FEB-T2.25', 'FED-23FEB-T2.50', 'FED-23FEB-T2.75', 'FED-23FEB-T3.00', 'FED-23FEB-T3.25', 'FED-23FEB-T3.50', 'FED-23FEB-T3.75', 'FED-23FEB-T4.00', 'FED-23FEB-T4.25', 'FED-23FEB-T4.50', 'FED-23FEB-T4.75', 'FED-23FEB-T5.00', 'FED-23FEB-T5.25', 
    'FED-23MAR-T3.00', 'FED-23MAR-T3.25', 'FED-23MAR-T3.50', 'FED-23MAR-T3.75', 'FED-23MAR-T4.00', 'FED-23MAR-T4.25', 'FED-23MAR-T4.50', 'FED-23MAR-T4.75', 'FED-23MAR-T5.00', 'FED-23MAR-T5.25',  'FED-23MAR-T5.50', 'FED-23MAR-T5.75', 'FED-23MAR-T6.00', 'FED-23MAR-T6.25', 'FED-23MAR-T6.50', 
    'FED-23MAY-T3.00', 'FED-23MAY-T3.25', 'FED-23MAY-T3.50', 'FED-23MAY-T3.75', 'FED-23MAY-T4.00', 'FED-23MAY-T4.25', 'FED-23MAY-T4.50', 'FED-23MAY-T4.75', 'FED-23MAY-T5.00', 'FED-23MAY-T5.25',  'FED-23MAY-T5.50', 'FED-23MAY-T5.75', 'FED-23MAY-T6.00', 'FED-23MAY-T6.25', 'FED-23MAY-T6.50', 
    'FED-23JUN-T3.00', 'FED-23JUN-T3.25', 'FED-23JUN-T3.50', 'FED-23JUN-T3.75', 'FED-23JUN-T4.00', 'FED-23JUN-T4.25', 'FED-23JUN-T4.50', 'FED-23JUN-T4.75', 'FED-23JUN-T5.00', 'FED-23JUN-T5.25',  'FED-23JUN-T5.50', 'FED-23JUN-T5.75', 'FED-23JUN-T6.00', 'FED-23JUN-T6.25', 'FED-23JUN-T6.50', 
    'FED-23JUL-T3.00', 'FED-23JUL-T3.25', 'FED-23JUL-T3.50', 'FED-23JUL-T3.75', 'FED-23JUL-T4.00', 'FED-23JUL-T4.25', 'FED-23JUL-T4.50', 'FED-23JUL-T4.75', 'FED-23JUL-T5.00', 'FED-23JUL-T5.25',  'FED-23JUL-T5.50', 'FED-23JUL-T5.75', 'FED-23JUL-T6.00', 'FED-23JUL-T6.25', 'FED-23JUL-T6.50', 
    'FED-23SEP-T3.00', 'FED-23SEP-T3.25', 'FED-23SEP-T3.50', 'FED-23SEP-T3.75', 'FED-23SEP-T4.00', 'FED-23SEP-T4.25', 'FED-23SEP-T4.50', 'FED-23SEP-T4.75', 'FED-23SEP-T5.00', 'FED-23SEP-T5.25',  'FED-23SEP-T5.50', 'FED-23SEP-T5.75', 'FED-23SEP-T6.00', 'FED-23SEP-T6.25', 'FED-23SEP-T6.50', 
    'FED-23NOV-T3.00', 'FED-23NOV-T3.25', 'FED-23NOV-T3.50', 'FED-23NOV-T3.75', 'FED-23NOV-T4.00', 'FED-23NOV-T4.25', 'FED-23NOV-T4.50', 'FED-23NOV-T4.75', 'FED-23NOV-T5.00', 'FED-23NOV-T5.25',  'FED-23NOV-T5.50', 'FED-23NOV-T5.75', 'FED-23NOV-T6.00', 'FED-23NOV-T6.25', 'FED-23NOV-T6.50', 
    'FED-23DEC-T3.00', 'FED-23DEC-T3.25', 'FED-23DEC-T3.50', 'FED-23DEC-T3.75', 'FED-23DEC-T4.00', 'FED-23DEC-T4.25', 'FED-23DEC-T4.50', 'FED-23DEC-T4.75', 'FED-23DEC-T5.00', 'FED-23DEC-T5.25',  'FED-23DEC-T5.50', 'FED-23DEC-T5.75', 'FED-23DEC-T6.00', 'FED-23DEC-T6.25', 'FED-23DEC-T6.50', 

    
    'FED-24JAN-T5.00', 'FED-24JAN-T5.25', 'FED-24JAN-T5.50', 'FED-24JAN-T5.75', 'FED-24JAN-T6.00',  'FED-24JAN-T6.25',  'FED-24JAN-T6.50',  
    'FED-24MAR-T4.50', 'FED-24MAR-T4.75', 'FED-24MAR-T5.00', 'FED-24MAR-T5.25', 'FED-24MAR-T5.50', 'FED-24MAR-T5.75', 'FED-24MAR-T6.00',  
    'FED-24MAY-T4.25', 'FED-24MAY-T4.50', 'FED-24MAY-T4.75', 'FED-24MAY-T5.00', 'FED-24MAY-T5.25', 'FED-24MAY-T5.50', 'FED-24MAY-T5.75', 
    'FED-24JUN-T4.00', 'FED-24JUN-T4.25', 'FED-24JUN-T4.50', 'FED-24JUN-T4.75', 'FED-24JUN-T5.00', 'FED-24JUN-T5.25', 'FED-24JUN-T5.50',
    'FED-24JUL-T4.00', 'FED-24JUL-T4.25', 'FED-24JUL-T4.50', 'FED-24JUL-T4.75', 'FED-24JUL-T5.00', 'FED-24JUL-T5.25', 'FED-24JUL-T5.50',
    'FED-24SEP-T3.75', 'FED-24SEP-T4.00', 'FED-24SEP-T4.25', 'FED-24SEP-T4.50', 'FED-24SEP-T4.75', 'FED-24SEP-T5.00', 'FED-24SEP-T5.25', 'FED-24SEP-T5.50', 
    'FED-24NOV-T3.50', 'FED-24NOV-T3.75', 'FED-24NOV-T4.00', 'FED-24NOV-T4.25', 'FED-24NOV-T4.50', 'FED-24NOV-T4.75', 'FED-24SEP-T5.00', 'FED-24NOV-T5.25', 'FED-24NOV-T5.50',
    'FED-24DEC-T3.25', 'FED-24DEC-T3.50', 'FED-24DEC-T3.75', 'FED-24DEC-T4.00', 'FED-24DEC-T4.25', 'FED-24DEC-T4.50', 'FED-24DEC-T4.75', 'FED-24DEC-T5.00', 'FED-24DEC-T5.25', 'FED-24DEC-T5.50',
    
    'FED-25JAN-T3.25', 'FED-25JAN-T3.50', 'FED-25JAN-T3.75', 'FED-25JAN-T4.00', 'FED-25JAN-T4.25', 'FED-25JAN-T4.50', 'FED-25JAN-T4.75', 'FED-22JAN-T5.00', 'FED-25JAN-T5.25', 'FED-25JAN-T5.50',   
    'FED-25MAR-T2.75', 'FED-25MAR-T3.00', 'FED-25MAR-T3.25', 'FED-25MAR-T3.50', 'FED-25MAR-T3.75', 'FED-25MAR-T4.00', 'FED-25MAR-T4.25', 'FED-25MAR-T4.50', 'FED-25MAR-T4.75', 'FED-25MAR-T5.00', 'FED-25MAR-T5.25', 
    'FED-25MAY-T2.75', 'FED-25MAY-T3.00', 'FED-25MAY-T3.25', 'FED-25MAY-T3.50', 'FED-25MAY-T3.75', 'FED-25MAY-T4.00', 'FED-25MAY-T4.25', 'FED-25MAY-T4.50', 'FED-25MAY-T4.75', 'FED-25MAY-T5.00', 'FED-25MAY-T5.25',
    'FED-25JUN-T2.75', 'FED-25JUN-T3.00', 'FED-25JUN-T3.25', 'FED-25JUN-T3.50', 'FED-25JUN-T3.75', 'FED-25JUN-T4.00', 'FED-25JUN-T4.25', 'FED-25JUN-T4.50', 'FED-25JUN-T4.75', 'FED-25JUN-T5.00', 'FED-25JUN-T5.25', 
    'FED-25JUL-T2.75', 'FED-25JUL-T3.00', 'FED-25JUL-T3.25', 'FED-25JUL-T3.50', 'FED-25JUL-T3.75', 'FED-25JUL-T4.00', 'FED-25JUL-T4.25', 'FED-25JUL-T4.50', 'FED-25JUL-T4.75', 'FED-25JUL-T5.00', 'FED-25JUL-T5.25', 'FED-25JUL-T5.50', 'FED-25JUL-T5.75', 'FED-25JUL-T6.00',  
    'FED-25SEP-T2.75', 'FED-25SEP-T3.00', 'FED-25SEP-T3.25', 'FED-25SEP-T3.50', 'FED-25SEP-T3.75', 'FED-25SEP-T4.00', 'FED-25SEP-T4.25', 'FED-25SEP-T4.50', 'FED-25SEP-T4.75', 'FED-25SEP-T5.00', 'FED-25SEP-T5.25', 'FED-25SEP-T5.50', 'FED-25SEP-T5.75', 'FED-25SEP-T6.00',  
    'FED-25OCT-T2.75', 'FED-25OCT-T3.00', 'FED-25OCT-T3.25', 'FED-25OCT-T3.50', 'FED-25OCT-T3.75', 'FED-25OCT-T4.00', 'FED-25OCT-T4.25', 'FED-25OCT-T4.50', 'FED-25OCT-T4.75', 'FED-25OCT-T5.00', 'FED-25OCT-T5.25', 'FED-25OCT-T5.50', 'FED-25OCT-T5.75', 'FED-25OCT-T6.00',  
    'FED-25DEC-T2.75', 'FED-25DEC-T3.00', 'FED-25DEC-T3.25', 'FED-25DEC-T3.50', 'FED-25DEC-T3.75', 'FED-25DEC-T4.00', 'FED-25DEC-T4.25', 'FED-25DEC-T4.50', 'FED-25DEC-T4.75', 'FED-25DEC-T5.00', 'FED-25DEC-T5.25', 'FED-25DEC-T5.50', 'FED-25DEC-T5.75', 'FED-25DEC-T6.00'
]


# loop through each ticker and get all the trades, append to the bottom of a pandas df

results = pd.DataFrame(columns=['trade_id', 'ticker', 'count', 'created_time', 'yes_price', 'no_price', 'taker_side'])


for ticker in tickers:
    print(f"Fetching: {ticker}")
    trades = client.get_trades(ticker=ticker)
    page_df = pd.DataFrame(trades['trades'])
    print(f"First page rows: {len(page_df)}")
    results = pd.concat([results, page_df], ignore_index=True)
    cursor = trades.get('cursor')

    page = 1
    while cursor:
        print(f"  Page {page} cursor: {cursor}")
        trades = client.get_trades(ticker=ticker, cursor=cursor)
        page_df = pd.DataFrame(trades['trades'])
        print(f"  Page {page} rows: {len(page_df)}")
        results = pd.concat([results, page_df], ignore_index=True)
        cursor = trades.get('cursor')
        page += 1

    time.sleep(1)
