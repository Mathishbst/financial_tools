# Structure de base pour une app Streamlit multi-pages sur la finance

# === streamlit_app.py (fichier principal) ===
import streamlit as st

st.set_page_config(page_title="App Financiere", layout="centered")
st.title("📊 Application Financiere")

st.markdown("Bienvenue dans votre outil d'analyse financiere !")
st.markdown("\n**Menu :**")
st.markdown("- 💰 Prix d'une obligation")
st.markdown("- 📊 MEDAF")
st.markdown("- 💼 Rendement de portefeuille")
st.markdown("- 📉 TRI (Taux de Rentabilité Interne)")

# === pages/1_Obligation.py ===
import streamlit as st

st.title("💰 Prix d'une obligation à taux fixe")

nominal = st.number_input("Valeur nominale (€)", value=1000)
coupon = st.number_input("Taux du coupon (%)", value=5.0) / 100
maturite = st.number_input("Durée (années)", value=5)
taux = st.number_input("Taux actuariel (%)", value=4.0) / 100

prix = sum([(nominal * coupon) / (1 + taux)**t for t in range(1, int(maturite) + 1)])
prix += nominal / (1 + taux)**int(maturite)

st.success(f"📌 Prix de l'obligation : {prix:.2f} €")

# === pages/2_MEDAF.py ===
import streamlit as st

st.title("📊 Calcul du rendement attendu selon le MEDAF")

tauxsansrisque = st.number_input("Taux sans risque (%)", value=3.0) / 100
rendementmarche = st.number_input("Rendement du marché (%)", value=10.0) / 100
beta = st.number_input("Bêta de l'actif", value=1.2)

medaf = (tauxsansrisque + beta * (rendementmarche - tauxsansrisque)) * 100

st.success(f"📌 Rendement attendu (MEDAF) : {medaf:.2f} %")

# === pages/3_Portefeuille.py ===
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

st.title("📉 Calcul du TRI (Taux de Rentabilité Interne)")

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
