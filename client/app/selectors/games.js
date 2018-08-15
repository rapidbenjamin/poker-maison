import Immutable from 'immutable';
import { createSelector } from 'reselect'

export const getGames = state => state.get('global').get('games')

export const getGame = tableName => createSelector(getGames, games => games.get(tableName))

export const getCurrentPlayerToAct = tableName => createSelector(
  getGame(tableName),
  game => game.get('_players')
    .indexOf(game.get('_currentPosToAct'))
)

export const isTurnToAct = (username, tableName) => createSelector(
  getCurrentPlayerToAct(tableName),
  player => player.get('_playerName') === username
)

export const getPlayerPos = (tableName, username) => createSelector(
  getGame(tableName), game => games.get('_players')
    .findIndex(
      plyr => plyr.get('_playerName') === username
    )
)


// In a two player game the small blind is the dealer position + 1
// In games with more than two players the small blind position is the dealer + 2
//
// if game stage not predeal or at least two players then fals
// if length players 2 then dealer +1 pos === playerPo
// else dealer +2 pos === playerPos
export const isBigBlind = (username, tableName) => createSelector(
  getGame(tableName), game => {
    return null
  })

// In a two player game the small blind is the dealer position 
// In games with more than two players the small blind position is the dealer + 1
//
// if game stage not predeal or at least two players then fals
// if length players 2 then dealer pos === playerPo
// else dealer +1 pos === playerPos
export const isSmallBlind = (username, tableName) => createSelector(
  getGame(tableName), game => {
    return null
  })