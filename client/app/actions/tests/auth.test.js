/* eslint-disable */
import configureMockStore from "redux-mock-store";
import thunk from "redux-thunk";
import axios from "axios";

import { authRequested, authError, authSuccess, login, logout, register } from "../auth";
import * as types from "../types";

const localStorageMock = {
  getItem: jest.fn(),
  setItem: jest.fn(),
  clear: jest.fn()
};

global.localStorage = localStorageMock;

const jestMock = response => jest
  .fn()
  .mockImplementation(
    () =>
      new Promise(
        (resolve, reject) =>
          response.status !== 200 ? reject(response) : resolve(response)
      )
  );

const stubAxios = response => {
  axios.get = jestMock(response)
  axios.post = jestMock(response)
};

describe("auth actions", () => {
  describe("action creators", () => {
    describe("authRequested", () => {
      it("should return correct action an authSuccess action for received asset", () => {
        expect(authRequested()).toEqual({ type: types.AUTH_REQUESTED })
      });
    })

    describe("authSuccess", () => {
      it("should return correct action an authSuccess action for received asset", () => {
        const username = 'Argo'
        expect(authSuccess(username)).toEqual({ type: types.AUTHENTICATED, username })
      });
    })

    describe("authError", () => {
      it("should return correct action an authSuccess action for received asset", () => {
        const error = '404'
        expect(authError(error)).toEqual({ type: types.AUTHENTICATION_ERROR, error })
      });
    })

    describe("logout", () => {
      it("should return correct action an authSuccess action for received asset", () => {
        expect(logout()).toEqual({ type: types.UNAUTHENTICATED })
      });
    })
  })

  describe("thunk actions", () => {
    let mockStore;
    let historyMock = { push: jest.fn() } // mocks react router history

    describe("signIn", () => {
      beforeEach(() => {
        const middlewares = [thunk];
        mockStore = configureMockStore(middlewares);
      });

      afterEach(() => {
        axios.get.mockReset();
        historyMock.push.mockReset()
        localStorage.clear()
      });

      afterAll(() => {
        axios.get.mockRestore();
      });

      const username = 'Argo'
      const email = 'email@email.com'
      const password = 'password'

      it("should dispatch correct actions when authentication succeeds", () => {
        const store = mockStore({});
        const expectedActions = [
          { type: types.AUTH_REQUESTED },
          { type: types.AUTHENTICATED }
        ];

        stubAxios({ status: 200, data: { token: 'JWT' } });
        return store.dispatch(login({ email, password }, historyMock)).then(() => {
          expect(store.getActions()).toEqual(expectedActions);
        });
      });


      it("should dispatch correction actions when error occurs while fetching user profile", () => {
        const store = mockStore({});
        const error = {
          "response": { "data": "Unauthorized" }, "status": 401
        }
        const expectedActions = [
          { type: types.AUTH_REQUESTED },
          { type: types.AUTHENTICATION_ERROR, error }
        ];

        stubAxios({ status: 401, response: { data: "Unauthorized" } });
        return store.dispatch(login({ email, password }, historyMock)).then(() => {
          expect(store.getActions()).toEqual(expectedActions);
        });
      });

      it("should redirect to correct route on auth success", () => {
        const store = mockStore({});
        const expectedRoute = '/lobby'
        stubAxios({ status: 200, data: { token: 'JWT' } });

        return store.dispatch(login({ email, password }, historyMock)).then(() => {
          expect(historyMock.push).toBeCalledWith(expectedRoute)
        });
      });

      it("should store JWT token in localStorage on auth success", () => {
        const store = mockStore({});
        const token = 'JWT'
        stubAxios({ status: 200, data: { token } });

        return store.dispatch(login({ email, password }, historyMock)).then(() => {
          expect(localStorage.setItem).toBeCalledWith('token', token)
        });
      });
    });
  });
});