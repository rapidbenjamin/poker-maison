# poker-server

Features

Staking API

Skill Ratings Generated for Players bb Won ratio over x hands

TODOs

***
- Use a map as data structure for players in poker games
- remove usage of FromJust from canBet, canRaise, canCall etc instead use find
  and throw player not at table err if Nothing
- add muck hand or show player action for players that win pot because everyone else folded
- wrap player moves on two timeout stages first one is 15 secs then 30 second countdown timer starts - notify players when each stage of countdown starts or if a player times out
- InvalidMoveErr errors should be refactored according to the comment above their definition
-- Add tests for getwinners to validate correct logic for SinglePlayerShowdown when allbut one folded and Winners 

- should construct your types so that illegal states are not representable
the goal is that if you ever have a value of type 'Game', it should be a legal value
ie.  game = StartedGame Game | NotStartedGame Game

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