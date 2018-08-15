import * as types from './types'

export const newGameState = (tableName, gameState) => ({ type: types.NEW_GAME_STATE, tableName, gameState })
