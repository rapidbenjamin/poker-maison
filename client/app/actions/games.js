import * as types from './types'

export const newGameState = (tableName, gameState) => ({
  type: types.NEW_GAME_STATE,
  tableName,
  gameState
})

export const postBigBlind = tableName => ({
  type: types.POST_BIG_BLIND,
  data: {
    tag: 'GameMsgIn',
    contents: {
      tag: 'GameMove',
      contents: [tableName, { tag: 'PostBlind', contents: 'Big' }]
    }
  }
})

export const postSmallBlind = tableName => ({
  type: types.POST_SMALL_BLIND,
  data: {
    tag: 'GameMsgIn',
    contents: {
      tag: 'GameMove',
      contents: [tableName, { tag: 'PostBlind', contents: 'Small' }]
    }
  }
})

export const bet = (tableName, amount) => ({
  type: types.BET,
  data: {
    tag: 'GameMsgIn',
    contents: {
      tag: 'GameMove',
      contents: [tableName, { tag: 'Bet', contents: Number(amount) }]
    }
  }
})

export const raise = (tableName, amount) => ({
  type: types.RAISE,
  data: {
    tag: 'GameMsgIn',
    contents: {
      tag: 'GameMove',
      contents: [tableName, { tag: 'Raise', contents: Number(amount) }]
    }
  }
})

export const call = tableName => ({
  type: types.CALL,
  data: {
    tag: 'GameMsgIn',
    contents: { tag: 'GameMove', contents: [tableName, { tag: 'Call' }] }
  }
})

export const check = tableName => ({
  type: types.CHECK,
  data: {
    tag: 'GameMsgIn',
    contents: { tag: 'GameMove', contents: [tableName, { tag: 'Check' }] }
  }
})

export const fold = tableName => ({
  type: types.FOLD,
  data: {
    tag: 'GameMsgIn',
    contents: { tag: 'GameMove', contents: [tableName, { tag: 'Fold' }] }
  }
})

export const leaveSeat = tableName => ({
  type: types.LEAVE_SEAT,
  data: {
    tag: 'GameMsgIn',
    contents: {
      tag: 'LeaveSeat',
      contents: tableName
    }
  }
})

export const sitIn = tableName => ({
  type: types.SIT_IN,
  data: { tag: 'GameMove', contents: [tableName, { tag: 'SitIn' }] }
})
