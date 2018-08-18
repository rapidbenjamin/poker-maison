/* 
   The data value of the action forms the websocket msg payload.
*/
import * as types from './types'

export const getLobby = () => ({ type: types.GET_LOBBY, data: { tag: 'GetTables' } })

export const newLobby = lobby => ({ type: types.NEW_LOBBY, lobby })

export const takeSeat = (tableName, chips) => ({
  type: types.TAKE_SEAT,
  data: {
    "tag": "TakeSeat", "contents": [tableName, Number(chips)]
  }
})

export const subscribeToTable = tableName => ({
  type: types.SUBSCRIBE_TO_TABLE,
  data: { "tag": "SubscribeToTable", "contents": tableName }
})

