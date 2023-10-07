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


try:
    async with session.get(
        url, headers=TRADE_DETAILS_HEADER, timeout=100
    ) as response:
        if response.status == 503:
            logger.info(
                f"Received 503 Service Unavailable on {date_obj}. Retrying..."
            )
            retry_count += 1
            await asyncio.sleep(1)
        else:
            response.raise_for_status()
            data = await response.json()
            logger.info(
                f"Successfully fetched trade details on {date_obj} from tse"
            )
            return [date_obj, pd.json_normalize(data["tradeHistory"])]
except (aiohttp.ClientError, asyncio.TimeoutError):
    logger.error(f"Request failed for {date_obj}. Retrying...")
    retry_count += 1
    await asyncio.sleep(1)
