
import streamlit as st

st.set_page_config(page_title="APP FINANCIAL", layout="centered")
st.title("📊 APP FINANCIAL")

st.markdown("Bienvenue dans votre outil d'analyse financiere !")
st.markdown("\n**Menu :**")
st.markdown("- 1ère partie : 💰 Prix d'une obligation")
st.markdown("- 2ème partie : 📊 MEDAF")
st.markdown("- 3ème partie : 💼 Rendement de portefeuille")
st.markdown("- 4ème partie : 📉 TRI (Taux de Rendement Interne)")
st.markdown("- 5ème partie : Cours des grands indices boursiers")

import streamlit as st

st.title("💰 Prix d'une obligation à taux fixe")

nominal = st.number_input("Valeur nominale (€)", value=1000)
coupon = st.number_input("Taux du coupon (%)", value=5.0) / 100
maturite = st.number_input("Durée (années)", value=5)
taux = st.number_input("Taux actuariel (%)", value=4.0) / 100

prix = sum([(nominal * coupon) / (1 + taux)**t for t in range(1, int(maturite) + 1)])
prix += nominal / (1 + taux)**int(maturite)

st.success(f"📌 Prix de l'obligation : {prix:.2f} €")

import streamlit as st

st.title("📊 Calcul du rendement attendu selon le MEDAF")

tauxsansrisque = st.number_input("Taux sans risque (%)", value=3.0) / 100
rendementmarche = st.number_input("Rendement du marché (%)", value=10.0) / 100
beta = st.number_input("Bêta de l'actif", value=1.2)

medaf = (tauxsansrisque + beta * (rendementmarche - tauxsansrisque)) * 100

st.success(f"📌 Rendement attendu (MEDAF) : {medaf:.2f} %")

import streamlit as st

st.title("💼 Rendement d'un portefeuille")

st.write("Entrez les rendements et poids de chaque actif")
n = st.number_input("Nombre d'actifs", min_value=1, max_value=20, value=5)

rendements = []
poids = []

for i in range(int(n)):
    r = st.number_input(f"Rendement actif {i+1} (%)", value=5.0, key=f"r{i}") / 100
    p = st.number_input(f"Poids actif {i+1} (entre 0 et 1)", value=0.2, key=f"p{i}")
    rendements.append(r)
    poids.append(p)

if sum(poids) > 0:
    rendement_portefeuille = sum(r * p for r, p in zip(rendements, poids)) * 100
    st.success(f"📌 Rendement du portefeuille : {rendement_portefeuille:.2f} %")
else:
    st.warning("⚠️ La somme des poids doit être supérieure à 0.")

# === pages/4_TRI.py ===
import streamlit as st
import numpy as np
import numpy_financial as npf
import matplotlib.pyplot as plt

st.title("📉 Calcul du TRI (Taux de Rendement Interne)")

st.write("Entrez les flux de trésorerie, y compris l'investissement initial (ex: -1000, 200, 300...)")

n = st.number_input("Nombre de périodes", min_value=1, max_value=30, value=5)
flux = []

for i in range(int(n) + 1):
    flux.append(st.number_input(f"Flux à t={i}", key=f"flux{i}"))

if len(flux) > 1:
    tri = npf.irr(flux) * 100
    st.success(f"📌 TRI estimé : {tri:.2f}%")

    tauxs = np.linspace(0.001, 1, 300)
    van = [sum([flux[t] / (1 + r)**t for t in range(len(flux))]) for r in tauxs]

    fig, ax = plt.subplots()
    ax.plot(tauxs * 100, van, label="VAN")
    ax.axhline(0, color="gray", linestyle="--")
    ax.axvline(tri, color="red", linestyle="--", label=f"TRI = {tri:.2f}%")
    ax.set_xlabel("Taux d'actualisation (%)")
    ax.set_ylabel("VAN")
    ax.set_title("Courbe de la VAN et TRI")
    ax.legend()
    ax.grid(True)
    st.pyplot(fig)

import streamlit as st
import yfinance as yf



import streamlit as st
import yfinance as yf

indices = {
    "S&P 500": "^GSPC",
    "Dow Jones": "^DJI",
    "NASDAQ": "^IXIC",
    "CAC 40": "^FCHI",
    "DAX": "^GDAXI",
    "IBEX 35": "^IBEX",
    "FTSE 100": "^FTSE"
}

if "selected_index" not in st.session_state:
    st.session_state.selected_index = None

if st.session_state.selected_index is None:
    st.title("🌍 Suivi des Grands Indices")

    cols = st.columns(3)
    for i, (name, ticker) in enumerate(indices.items()):
        data = yf.Ticker(ticker)
        price = data.history(period="1d")["Close"][-1]

        with cols[i % 3]:
            st.markdown(f"### **{name}**")
            st.markdown(f"## **💰 {price:.2f}**")
            if st.button(f"Voir détail {name}", key=name):
                st.session_state.selected_index = name

else:
    name = st.session_state.selected_index
    ticker = indices[name]
    data = yf.Ticker(ticker)
    hist = data.history(period="1y")
    price = data.history(period="1d")["Close"][-1]
    previous_close = data.history(period="2d")["Close"][0]
    change = price - previous_close
    pct_change = (change / previous_close) * 100

    st.button("⬅️ Retour", on_click=lambda: st.session_state.update({"selected_index": None}))

    st.title(f"📈 {name}")
    st.metric(label="Cours actuel", value=f"{price:.2f}", delta=f"{pct_change:.2f}%")

    st.subheader("Historique sur 1 an")
    st.line_chart(hist["Close"])

    st.dataframe(hist.tail(10))

import streamlit as st
import yfinance as yf



stocks = {
    "Apple": "AAPL",
    "Microsoft": "MSFT",
    "Tesla": "TSLA",
    "Amazon": "AMZN",
    "Nvidia": "NVDA",
    "Airbus": "AIR.PA",
    "LVMH": "MC.PA",
    "Air Liquide": "AI.PA",
    "Thales": "HO.PA",
    "Samsung Electronics": "005930.KS",
    "TotalEnergies": "TTE.PA",
    "Visa": "V",
    "PayPal": "PYPL",
    "Hermès": "RMS.PA",
    "AXA": "CS.PA",
    "Goldman Sachs": "GS",
    "Stellantis": "STLA",  # ou STLAP.PA
    "Alibaba Group": "BABA",
    "Accor": "AC.PA",
    "Amundi": "AMUN.PA",
    "Alphabet (C)": "GOOG",
    "Nintendo": "7974.T",
    "Walmart": "WMT",
    "Spotify": "SPOT"
}

if "selected_stock" not in st.session_state:
    st.session_state.selected_stock = None

if st.session_state.selected_stock is None:
    st.title("📈 Watchlist Actions")

    cols = st.columns(3)
    for i, (name, ticker) in enumerate(stocks.items()):
        data = yf.Ticker(ticker)
        try:
            price = data.history(period="1d")["Close"][-1]
        except:
            price = 0.0  # Gérer les cas sans donnée

        with cols[i % 3]:
            st.markdown(f"### **{name}**")
            st.markdown(f"## **💵 {price:.2f}**")
            if st.button(f"Voir détail {name}", key=name):
                st.session_state.selected_stock = name

else:
    name = st.session_state.selected_stock
    ticker = stocks[name]
    data = yf.Ticker(ticker)
    hist = data.history(period="1y")
    price = data.history(period="1d")["Close"][-1]
    previous_close = data.history(period="2d")["Close"][0]
    change = price - previous_close
    pct_change = (change / previous_close) * 100

    st.button("⬅️ Retour", on_click=lambda: st.session_state.update({"selected_stock": None}))

    st.title(f"📊 {name}")
    st.metric(label="Cours actuel", value=f"{price:.2f}", delta=f"{pct_change:.2f}%")

    st.subheader("Historique sur 1 an")
    st.line_chart(hist["Close"])

    st.dataframe(hist.tail(10))

