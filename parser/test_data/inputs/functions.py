def _handle_ticker_index(symbol):
    ticker_index = symbols_data.get_ticker_index(symbol)

    if ticker_index is None:
        market_symbol = get_symbol_info(symbol)
        if market_symbol is not None:
            symbols_data.append_symbol_to_file(market_symbol)
            ticker_index = market_symbol.index
    return ticker_index


def _extract_ticker_client_types_data(ticker_index: str) -> List:
    url = TSE_CLIENT_TYPE_DATA_URL.format(ticker_index)
    with requests_retry_session() as session:
        response = session.get(url, timeout=5)
    data = response.text.split(';')
    return data

def common_process(df: pd.DataFrame, date: str):
    if len(df) == 0:
        return pd.DataFrame(columns=list(api_to_orderbook_mapping.keys()))
    df.rename(columns=reversed_keys, inplace=True)
    df = df.loc[:, list(api_to_orderbook_mapping.keys())]
    df["datetime"] = pd.to_datetime(
        date + " " + df["datetime"].astype(str), format="%Y%m%d %H%M%S"
    )
    df = df.sort_values(["datetime", "depth"], ascending=[True, True])
    df.set_index("datetime", inplace=True)
    df.drop(columns=["refID"], inplace=True)
    return df

