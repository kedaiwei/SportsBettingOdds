import requests
from bs4 import BeautifulSoup
import pandas as pd

URL = "https://www.covers.com/sport/odds"
page = requests.get(URL)

soup = BeautifulSoup(page.content, "html.parser")

odds_hub = soup.find("div", class_="__oddsHubCont")
leagues = odds_hub.find_all("h2", class_="__leagueName")

league_dict = {}
for league in leagues:
    span = league.find("span")
    league_name = (span.get_text()).strip()
    league_anchor = league.find("a", class_="__cta")
    league_link = league_anchor["href"]
    league_dict[league_name] = league_link


def has_moneyline_id(tag):
    required_classes = set([
        "table", "__OpenOddsTable", "covers-CoversMatchups-Table",
        "covers-CoversOdds-gamelineTable",
        "covers-CoversComponents-fixedColumn"
    ])
    return (tag.name == "table" and tag.get("id") and "moneyline" in tag["id"]
            and tag.get("class")
            and required_classes.issubset(set(tag["class"])))


def has_threeway_id(tag):
    required_classes = set([
        "table", "__OpenOddsTable", "covers-CoversMatchups-Table",
        "covers-CoversOdds-gamelineTable",
        "covers-CoversComponents-fixedColumn"
    ])
    return (tag.name == "table" and tag.get("id")
            and "threeway-game" in tag["id"] and tag.get("class")
            and required_classes.issubset(set(tag["class"])))


def is_odds_table(tag):
    required_classes = set([
        "table", "__OddsTable", "covers-CoversMatchups-Table",
        "covers-CoversOdds-gamelineTable",
        "covers-CoversComponents-fixedHeaderTable"
    ])
    return (tag.name == "table" and tag.get("class")
            and required_classes.issubset(set(tag["class"])))


# Determines whether or not a 'tag' is a grid cell containing odds for the appropriate game, denoted by 'game_no'
def is_odd_for_game(game_no, tag):
    return (tag.name == "td" and "liveOddsCell" in tag["class"]
            and tag.get('data-game') and tag["data-game"] == game_no)


league_odds = {}
for name, link in league_dict.items():
    league_page = requests.get("https://www.covers.com" + link)
    sub_soup = BeautifulSoup(league_page.content, "html.parser")
    # Get all games in a list
    table = sub_soup.find(has_moneyline_id)
    if not table:
        table = sub_soup.find(has_threeway_id)
    rows = table.find_all(lambda tag: tag.name == "tr" and tag.get("class") and
                          "oddsGameRow" in tag["class"])
    game_dic = {}
    for row in rows:
        if not row.find("div", class_="__isCompleted"
                        ):  # Checking to see that game is not finished
            teams = row.find(lambda tag: tag.name == "div" and tag.get("class")
                             and "__teams" in tag["class"])
            # Get home team
            home = teams.find("div", class_="__home")
            home_anchor = home.find("a")
            home_team = home_anchor.get_text()
            # Get away team
            away = teams.find("div", class_="__away")
            away_anchor = away.find("a")
            away_team = away_anchor.get_text()
            button = row.find("button")
            game_no = button["data-game"]
            game_dic[game_no] = (home_team.strip() + " vs. " +
                                 away_team.strip())

    # Get list of odds for given game (site will be a string, odds will be in list form [W, L, Some D])
    # odds_table = sub_soup.find(is_odds_table)
    matchup_dic = {}
    for num, matchup in game_dic.items():
        grid_cells = sub_soup.findAll(  # Finds everything neccessary
            lambda tag: tag.name == "td" and tag.get("class") and
            "liveOddsCell" in tag["class"] and tag.get("data-game") and
            tag["data-game"] == num and tag.get("data-type") and (tag[
                "data-type"] == "threeway" or tag["data-type"] == "moneyline"))
        odds_dic = {}
        for cell in grid_cells:
            odds = []
            murica_away_odds_text = ""
            murica_home_odds_text = ""
            murica_draw_odds_text = ""
            site = cell["data-book"]
            away_odds = cell.find("div", class_="__awayOdds")
            if away_odds:
                murica_away_odds = away_odds.find(
                    lambda tag: tag.name == "div" and "__american" in tag[
                        "class"])
                murica_away_odds_text = murica_away_odds.get_text()
            home_odds = cell.find("div", class_="__homeOdds")
            if home_odds:
                murica_home_odds = home_odds.find(
                    lambda tag: tag.name == "div" and "__american" in tag[
                        "class"])
                murica_home_odds_text = murica_home_odds.get_text()
            draw_odds = cell.find("div", class_="__drawOdds")
            if draw_odds:
                murica_draw_odds = draw_odds.find(
                    lambda tag: tag.name == "div" and "__american" in tag[
                        "class"])
                murica_draw_odds_text = murica_draw_odds.get_text()
                odds = [
                    murica_away_odds_text, murica_home_odds_text,
                    murica_draw_odds_text
                ]
            else:
                odds = [murica_away_odds_text, murica_home_odds_text]
            odds_dic[site] = odds
        matchup_dic[matchup] = odds_dic
    league_odds[name] = matchup_dic

# Flatten the nested dictionary
data = []

for league, games in league_odds.items():
    for game, sites in games.items():
        for site, odds in sites.items():
            data.append([league, game, site, odds])

# Create the DataFrame from the flattened dictionary
df_gambling = pd.DataFrame(data, columns=['League', 'Game', 'Site', 'Odds'])
df_gambling.to_json("./data/out.json")
