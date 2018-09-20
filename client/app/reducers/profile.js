import Immutable from 'immutable'

import * as types from '../actions/types'

const initialState = Immutable.Map({
  profile: undefined,
  isLoading: false,
  error: null
})

export default function(state = initialState, action) {
  switch (action.type) {
    case types.GET_PROFILE_REQUEST:
      return state.set('isLoading', true)
    case types.GET_PROFILE_SUCCESS:
      return state
        .set('username', action.profile)
        .set('isLoading', false)
        .set('error', null)
    case types.GET_PROFILE_ERR:
      return state.set('error', action.error).set('isLoading', false)
    default:
      return state
  }
}
