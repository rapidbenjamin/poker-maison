import { createSelector } from 'reselect'

export const getGames = state => state.get('global').get('games')

export const getGame = tableName =>
  createSelector(getGames, games => games.get(tableName))

export const getCurrentPlayerToAct = tableName => (
  getGame(tableName),
  game => {
    if (!game) {
      return null
    }

    const currentPosToAct = game.get('_currentPosToAct')

    if (Number.isInteger(currentPosToAct)) {
      const player = game.get('_players').get(currentPosToAct)

      if (player.get("_hasActed") == true) {
        return null
      } else {
        return player.get('_playerName')
      }
    }

    return null
  }
)

export const isTurnToAct = (username, tableName) => createSelector(
  getCurrentPlayerToAct(tableName),
  playerName => playerName === username
)

export const getPlayerPosition = (tableName, username) => createSelector(
  getGame(tableName), game => {
    if (!game) return null
    return game.get('_players')
      .findIndex(
        plyr => plyr.get('_playerName') === username
      )
  }
)
