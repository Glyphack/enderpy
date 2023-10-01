# in some cases last price or adj price is undefined
try:
    last_price = int(price_section[2])
# when instead of number value is `F`
except (ValueError, IndexError):
    last_price = None
try:
    adj_close = int(price_section[3])
except (ValueError, IndexError):
    adj_close = None
try:
    market_cap = adj_close * self.total_shares
except ValueError:
    market_cap = None

