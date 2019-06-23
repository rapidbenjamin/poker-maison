/* 
   The data value of the action forms the websocket msg payload.
*/
import * as types from './types'

export const getLobby = () => ({
  type: types.GET_LOBBY,
  data: { tag: 'GetTables' }
})

export const newLobby = lobby => ({ type: types.NEW_LOBBY, lobby })

// should be moved as this is game action
export const takeSeat = (tableName, chips) => ({
  type: types.TAKE_SEAT,
  data: {
    tag: 'GameMsgIn',
    contents: {
      tag: 'TakeSeat',
      contents: [tableName, Number(chips)]
    }
  }
})

export const subscribeToTable = tableName => ({
  type: types.SUBSCRIBE_TO_TABLE,
  data: { tag: 'SubscribeToTable', contents: tableName }
})
