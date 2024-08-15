from bs4 import BeautifulSoup
from selenium import webdriver
import csv
import time

# ouvre une fenêtre de Chromium
driver = webdriver.Chrome("/Users/leopicat/chromedriver")
driver.get("https://galien.cngsante.fr")

# liste pour stocker les données
container = []

for i in range(25):
    # extrait l'html de la page
    html = driver.page_source
    soup = BeautifulSoup(html)
    # extrait toutes les tables de la page 
    tables = soup.find_all("table-generic")
    table = tables[0]
    # parcourt la table pour extraire ce qui m'intéresse
    # 0 groupe de spé, 1 spé, 2 ville, 3 poste dispo, 4 poste pourvu, 5 rang limite
    for row in table.find_all("datatable-body-row"):
        cells = row.find_all("datatable-body-cell")
        container.append([cells[0].text, cells[1].text, cells[2].text, cells[3].text, cells[4].text, cells[5].text])
    # passe à la page suivante
    driver.find_element_by_css_selector('[aria-label="go to next page"]').click()
    # attends que la table se charge
    time.sleep(3)

# sauvegarde les données
with open('simulation-tour-6.csv', 'w', newline='') as f:
    write = csv.writer(f)
    write.writerows(container)