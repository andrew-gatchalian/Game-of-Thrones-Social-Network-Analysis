# Game of Thrones Social Network Analysis
Dissect the intricate world of George R.R. Martin's A Song of Ice and Fire, also famously known as HBO's Game of Thrones! Discover how characters like Tyrion Lannister and Daenerys Targaryen evolve and navigate through the saga's sprawling social network! Created by Sindu Bhavanam, Andrew Gatchalian, Hyo Won Lee, Dimitrios Mousouroulis, Kuan-I Wu at the University of California, Irvine.

- Analyzed character co-occurrence data using R to visualize relationships and power dynamics within the A Song of Ice and Fire book series.
- Implemented centrality measures (degree, closeness, betweenness, eigenvector) to identify the evolving influence of key characters across the saga.

## Purpose
The purpose of this project is to uncover and analyze the “social complexities” of the series through
Social Network Analysis (SNA), which is a tool that is often used in sociology to understand
relationships and interactions within a network. By applying social network analysis to "Game of
Thrones," we aim to quantify and visualize the relationships between characters and explore how
these relationships develop throughout the series and more specifically from books one through
five. This analysis aims to shed light on character importance and story progression and also
attempt to identify patterns that might explain the story's depth and the show's huge success.
Furthermore, this analysis aims to demonstrate how character inter-connectivity can affect the
audience’s or fan’s engagement and the overall success of storytelling.

## Data
The primary source of data is a comprehensive collection of chapters from all published books in
the series, which were designed to record each instance of character interaction. We came across a
very rich dataset on [Github](https://github.com/mathbeveridge/asoiaf/tree/master/data) with character interaction networks, which contain all the main and
supporting characters across the “A Song of Ice and Fire” book series and focuses on their
interactions throughout the books.
The authors behind the datasets, created these networks by connecting two characters whenever
their nicknames or actual names appeared within fifteen words of one another (referred to as
“co-occurrence”) in books one through five within the ‘ASOIAF’ series, with the weight of the edges
corresponding to the amount of interactions.
