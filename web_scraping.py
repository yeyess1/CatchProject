import requests
from urllib.parse import urljoin
import json
from bs4 import BeautifulSoup
from typing import Optional, List, Dict, Any
import pandas as pd


api_key = "57d5f0a1-3d3c-45d1-bc20-bdc4405946cb"
url_brigh = "https://api.brightdata.com/request"


def link_maker(lugar: str, industria: str, country: str = "mx", fragment: str | None = None) -> str:
    """
    Concatena la URL de Computrabajo con valores ya formateados (slugs).
    No modifica los parámetros; solo arma el enlace.

    Ej: lugar="estado-de-mexico", industria="manufactura"
    -> https://mx.computrabajo.com/trabajo-de-manufactura-en-estado-de-mexico
    """
    base = f"https://{country}.computrabajo.com/trabajo-de-{industria}-en-{lugar}"
    if fragment:
        frag = fragment.lstrip("#")  # por si te llega con '#'
        return f"{base}#{frag}"
    return base


def _get(url, proxy=None, **kwargs):
    """Wrapper para session.get: consume api de Brighdata para evitar bloqueos por login o Captchas."""
    headers = {
        "Authorization": "Bearer {}".format(api_key),
        "Content-Type": "application/json"
    }

    data = {
        "zone": "web_unlockercastor",
        "url": url,
        "format": "raw"
    }
    response = requests.post(url_brigh, json=data, headers=headers)

    return response


def scraping(
    k: Optional[int],
    lugar: str,
    industria: str,
    country: str = "mx",
    fragment: Optional[str] = None,
) -> pd.DataFrame:
    """
    Extrae hasta k ofertas de Computrabajo, incluyendo empresa y ubicación,
    y guarda los resultados en un CSV.
    """
    base_url = f"https://{country}.computrabajo.com"
    search_url = link_maker(lugar, industria, country, fragment)

    response = _get(search_url)
    soup = BeautifulSoup(response.text, "lxml")

    container = soup.select_one("#offersGridOfferContainer")
    if not container:
        return pd.DataFrame()

    companies: List[Dict[str, Any]] = []


    for article in container.select("article.box_offer"):
        # Empresa
        a = article.select_one("p.dFlex.vm_fx.fs16.fc_base.mt5 a.fc_base.t_ellipsis[href]")
        if not a:
            a = article.select_one('a[offer-grid-article-company-url][href]')
        name = a.get_text(strip=True) if a else None
        company_url = urljoin(base_url, a["href"]) if a else None

        location_span = article.select_one("p.fs16.fc_base.mt5 span.mr10")
        location = location_span.get_text(strip=True) if location_span else None

        if name or location:
            companies.append({
                "company": name,
                "url": company_url,
                "location": location
            })

            if isinstance(k, int) and k > 0 and len(companies) >= k:
                break


    df = pd.DataFrame(companies)
    df.to_csv("companies.csv", index=False, encoding="utf-8-sig")
    print("Datos guardados en 'companies.csv'")
    return df


scraping(10, 'ciudad-de-mexico', 'manufactura', )
  
  


  

