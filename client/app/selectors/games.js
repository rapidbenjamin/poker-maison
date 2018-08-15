import Immutable from 'immutable';
import { createSelector } from 'reselect'

export const getGames = state => state.get('global').get('games')

export const getGame = tableName => createSelector(getGames, games => games.get(tableName))

export const isTurnToAct = (username, tableName) => createSelector(
  getGame(tableName), game => {
    if (game === undefined || game.isEmpty) {
      return false
    }
    const currentPlayerToAct = game.get('_players').indexOf(game.get('_currentPosToAct'))
    const currentPlayerNameToAct = currentPlayerToAct.get('_playerName')

    return username === currentPlayerNameToAct
  })