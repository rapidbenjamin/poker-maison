# poker-server

Features

Staking API

Skill Ratings Generated for Players bb Won ratio over x hands

TODOs

***
- Use a map as data structure for players in poker games
- remove usage of FromJust from canBet, canRaise, canCall etc instead use find
  and throw player not at table err if Nothing

**
- Lensify use of maps
- For the PlayerState Types Out AllIn doesn't make sense. Change Type to In AllIn
as out means that a player cannot win the hand however an all in player still has
the possibility of winning. Will need to update actions to mark to new state as well
as any time a new player is set.
- Rename PreDeal stage to Blinds

*
- Delay of 3 seconds between each game stage for UX
- Use System.Timeout to wrap player actions in timeout, will need to add a new msg signalling
  to all table subscribes that the player has timed out