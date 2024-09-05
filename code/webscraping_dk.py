import requests
from bs4 import BeautifulSoup
import pandas as pd

# Step 1: Send a request to the website
url = 'https://sportsbook.draftkings.com/leagues/football/nfl'
response = requests.get(url)

# Check if the request was successful
if response.status_code == 200:

    # Step 2: Parse the HTML content
    soup = BeautifulSoup(response.content, 'html.parser')

    # Step 3: Find elements with the specific class 'sportsbook-outcome-cell__body no-label'
    # This will get the div for the moneyline
    elements = soup.find_all('div', class_='sportsbook-outcome-cell__body no-label')

    # Create Dummy data variable for future storage
    data = []

    # Loop through each element and extract 'aria-label' and corresponding odds
    # 'aria-label' contains the team name.
    for element in elements:
        # Extract the 'aria-label' value
        team = element.get('aria-label', '')

        # Find the corresponding odds within the same div
        odds_element = element.find('span', class_='sportsbook-odds american no-margin default-color')
        odds = odds_element.get_text(strip=True) if odds_element else ''

        # Append the data to the list
        # For some reason the spreads get pulled with the label
        # Adding the if odds != '' ensures only the ML odds are pulled with the team name
        if odds != '':
            data.append([team, odds])

    # Step 4: Output the data in a table format using Pandas
    df = pd.DataFrame(data, columns=['team', 'moneyline'])

    # Save the data to a CSV file
    csv_filename = 'dk_moneylines_9_4_2024.csv'
    df.to_csv(csv_filename, index=False)
    print(f"Data saved to {csv_filename}")
else:
    print(f"Failed to retrieve the webpage. Status code: {response.status_code}")

