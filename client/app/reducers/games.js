import Immutable from 'immutable';

import * as types from '../actions/types';

const initialState = Immutable.Map({});

export default function (state = initialState, action) {
    switch (action.type) {
        case types.AUTH_REQUESTED:
            return state.set('isLoading', true);
        case types.AUTHENTICATED:
            return state.set('authenticated', true).set('username', action.username).set('isLoading', false).set('error', null);
        case types.UNAUTHENTICATED:
            return state.set('authenticated', false).set('username', null).set('isLoading', false).set('error', null);
        case types.AUTHENTICATION_ERROR:
            return state.set('error', action.error).set('isLoading', false)
        default:
            return state
    }
}