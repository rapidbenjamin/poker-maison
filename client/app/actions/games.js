import * as types from './types'

export const newGameState = (tableName, gameState) => ({
  type: types.NEW_GAME_STATE,
  tableName,
  gameState
})

export const postBigBlind = tableName => ({
  type: types.POST_BIG_BLIND,
  data: { "tag": "GameMove", "contents": [tableName, { "tag": "PostBlind", "contents": "Big" }] }
})

export const postSmallBlind = tableName => ({
  type: types.POST_SMALL_BLIND,
  data: { "tag": "GameMove", "contents": [tableName, { "tag": "PostBlind", "contents": "Small" }] }
})