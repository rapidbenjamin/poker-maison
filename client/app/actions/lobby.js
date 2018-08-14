import * as types from './types'

export const getLobby = () => ({ type: types.GET_LOBBY, data: { tag: 'GetTables' } })

export const newLobby = lobby => ({ type: types.NEW_LOBBY, lobby })
