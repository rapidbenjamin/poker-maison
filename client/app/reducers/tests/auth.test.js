/* eslint-disable */
import { Map, fromJS } from "immutable";
import reducer from "../auth";
import * as actions from "../../actions/index";

const initialState = Map({
  authenticated: false,
  username: null,
  error: null
});

describe("auth reducer", () => {
  it("should return the initial state", () => {
    expect(reducer(undefined, {})).toEqual(initialState);
  });

  it("should handle authentication success", () => {
    const expectedState = Map({
      isAuthenticated: true,
      username: 'Argo',
      error: null
    });

    expect(reducer(initialState, actions.authSuccess(expectedState.username))).toEqual(expectedState);
  });


  it("should handle authentication errors", () => {
    const expectedState = Map({
      isAuthenticated: false,
      username: null,
      error: '404'
    });

    expect(reducer(initialState, actions.authError(expectedState.error))).toEqual(expectedState);
  });

  it("should handle logout", () => {
    const initialState = Map({
      authenticated: true,
      username: 'Argo',
      error: null
    });
    const expectedState = Map({
      isAuthenticated: false,
      username: null,
      error: null
    });

    expect(reducer(initialState, actions.logout())).toEqual(expectedState);
  });
});