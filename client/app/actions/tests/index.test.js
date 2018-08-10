/* eslint-disable */
import configureMockStore from "redux-mock-store";
import thunk from "redux-thunk";
import axios from "axios";

import * as actions from "../index";
import * as types from "../types";

const stubAxios = response => {
  axios.get = jest
    .fn()
    .mockImplementation(
      () =>
        new Promise(
          (resolve, reject) =>
            response.status !== 200 ? reject(response) : resolve(response)
        )
    );
};

describe("actions", () => {
  it("should create an action for received asset", () => {
    const id = "gi4uH53s";
    const name = "car";
    const comment = "lorem ipsum";
    const action = actions.receiveAsset(id, name, comment);

    expect(action.type).toEqual(types.RECEIVE_ASSET);
    expect(typeof action.id).toEqual("string");
    expect(typeof action.timestamp).toEqual("string");
  });

  it("should create an action to dispatch asset", () => {
    const id = "gi4uH53s";
    const action = actions.dispatchAsset(id);

    expect(action.type).toEqual(types.DISPATCH_ASSET);
    expect(typeof action.id).toEqual("string");
    expect(typeof action.timestamp).toEqual("string");
  });

  it("should create an action to edit asset name", () => {
    const id = "gi4uH53s";
    const newName = "truck";
    const expectedAction = {
      type: types.EDIT_ASSET_NAME,
      id,
      newName
    };

    expect(actions.editAssetName(id, newName)).toEqual(expectedAction);
  });

  it("should create an action to edit asset comment", () => {
    const id = "gi4uH53s";
    const newComment = "on road";
    const expectedAction = {
      type: types.EDIT_ASSET_COMMENT,
      id,
      newComment
    };

    expect(actions.editAssetComment(id, newComment)).toEqual(expectedAction);
  });

  it("should create an authSuccess action", () => {
    const expectedAction = {
      type: types.AUTH_SUCCESS
    };

    expect(actions.authSuccess()).toEqual(expectedAction);
  });

  it("should create a loggedOut action", () => {
    const expectedAction = {
      type: types.LOGGED_OUT
    };

    expect(actions.loggedOut()).toEqual(expectedAction);
  });

  describe("requestProfile", () => {
    it("should create a requestProfile action", () => {
      const expectedAction = {
        type: types.REQUEST_PROFILE
      };

      expect(actions.requestProfile()).toEqual(expectedAction);
    });
  });

  describe("receiveProfile", () => {
    it("should create a receiveProfile action", () => {
      const data = { firstName: "Foo", lastName: "Bar" };
      const expectedAction = {
        type: types.RECEIVE_PROFILE,
        data
      };

      expect(actions.receiveProfile(data)).toEqual(expectedAction);
    });
  });

  describe("requestProfileErr", () => {
    it("should create a requestProfileErr action", () => {
      const err = "404";
      const expectedAction = {
        type: types.REQUEST_PROFILE_ERR,
        err
      };

      expect(actions.requestProfileErr(err)).toEqual(expectedAction);
    });
  });

  describe("fetchProfile", () => {
    let mockStore;
    const profile = {
      sub: "42768269",
      name: "Foo"
    };

    beforeEach(() => {
      const middlewares = [thunk];
      mockStore = configureMockStore(middlewares);
    });

    afterEach(() => {
      axios.get.mockReset();
    });

    afterAll(() => {
      axios.get.mockRestore();
    });

    it("should dispatch correct actions when user profile is fetched successfully", () => {
      const store = mockStore({});
      const expectedActions = [
        { type: types.REQUEST_PROFILE },
        { type: types.RECEIVE_PROFILE, data: profile }
      ];

      stubAxios({ status: 200, data: profile });

      return store.dispatch(actions.fetchProfile()).then(() => {
        expect(store.getActions()).toEqual(expectedActions);
      });
    });

    it("should pass auth token correctly in http header options", () => {
      const expectedHeader = {
        headers: {
          authorization: "Bearer h73",
          "Content-Type": "application/json"
        }
      };
      const token = "h73";
      const store = mockStore({});

      stubAxios({ status: 200, data: profile });

      return store.dispatch(actions.fetchProfile(token)).then(() => {
        const expectedHeader = {};
        const headers = axios.get.mock.calls[0][1]["headers"];

        expect(headers.authorization).toEqual("Bearer h73");
      });
    });

    it("should dispatch correction actions when error occurs while fetching user profile", () => {
      const store = mockStore({});
      const expectedActions = [
        { type: types.REQUEST_PROFILE },
        { type: types.REQUEST_PROFILE_ERR, err: { data: "Unauthorized" } }
      ];

      stubAxios({ status: 401, response: { data: "Unauthorized" } });

      return store.dispatch(actions.fetchProfile()).then(() => {
        expect(store.getActions()).toEqual(expectedActions);
      });
    });
  });
});