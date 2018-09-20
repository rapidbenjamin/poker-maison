import axios from 'axios'

import * as types from './types'

export const getProfileRequest = () => ({ type: types.GET_PROFILE_REQUEST })

export const getProfileSuccess = profile => ({
  type: types.GET_PROFILE_SUCCESS,
  profile
})

export const getProfileErr = error => ({ type: types.GET_PROFILE_ERR, error })

export const getProfile = username => {
  return async dispatch => {
    const token = localStorage.getItem('token')
    if (token) {
      try {
        const { access_token } = JSON.parse(token)
      } catch (e) {
        console.log(e)
      }

      // TODO ADD HEADER TO AUTH
      axios
        .post(`${AUTH_API_URL}/profile`)
        .then(({ data }) => {
          const { profile } = data
          dispatch(getProfileSuccess(username))
        })
        .catch(err => dispatch(getProfileErr(err)))
    } else {
      throw Error('No JWT token for profile request')
    }
  }
}
