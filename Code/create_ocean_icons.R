# Create ocean icons
oceanIcons <- iconList(
  ship = makeIcon(
    "https://upload.wikimedia.org/wikipedia/commons/thumb/0/0b/Maki2-ferry-18.svg/480px-Maki2-ferry-18.svg.png",
    "https://upload.wikimedia.org/wikipedia/commons/thumb/0/0b/Maki2-ferry-18.svg/18px-Maki2-ferry-18.svg.png",
    18,
    18
  ),
  pirate = makeIcon(
    "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c3/Maki2-danger-24.svg/240px-Maki2-danger-24.svg.png",
    "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c3/Maki2-danger-24.svg/24px-Maki2-danger-24.svg.png",
    24,
    24
  )
)

# Save icons
save(oceanIcons, file = here::here("Images/oceanIcons.Rdata"))
