# Create ocean icons
oceanIcons <- iconList(
  ship = makeIcon(
    "https://upload.wikimedia.org/wikipedia/commons/0/09/Ship_-_The_Noun_Project.svg",
    "https://upload.wikimedia.org/wikipedia/commons/0/09/Ship_-_The_Noun_Project.svg",
    24,
    24
  ),
  pirate = makeIcon(
    "https://upload.wikimedia.org/wikipedia/commons/0/06/Skull_and_Crossbones.svg",
    "https://upload.wikimedia.org/wikipedia/commons/0/06/Skull_and_Crossbones.svg",
    24,
    24
  )
)

# Save icons
save(oceanIcons, file = here::here("Images/oceanIcons.Rdata"))

# https://upload.wikimedia.org/wikipedia/commons/0/09/Ship_-_The_Noun_Project.svg
