import Immutable from 'immutable';
import {
    combineReducers
} from 'redux-immutable';

function toDoApp(state = initialState, action) {
    // For now, don't handle any actions
    // and just return the state given to us.
    return state
}


const initialState = Immutable.Map();


export default toDoApp
