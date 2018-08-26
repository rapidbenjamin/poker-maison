import React from 'react'

const ActionPanel = ({
  handleChange,
  betValue,
  bet,
  raise,
  call,
  fold,
  check,
  postSmallBlind,
  postBigBlind,
  sitDown,
  leaveGameSeat
}) =>
  (<div className='actionPanel'>
    <button
      type="button"
      onClick={() => postBigBlind()} className="button">
      postBigBlind
    </button>
    <button
      type="button" onClick={() => postSmallBlind()} className="button">
      postSmallBlind
    </button>
    <button type="button" onClick={() => check()} className="button">
      check
    </button>
    <button type="button" onClick={() => call()} className="button">
      call</button>
    <button
      type="button"
      onClick={() => bet(betValue)} className="button">Bet {betValue}</button>
    <button
      type="button"
      onClick={() => raise(betValue)}
      className="button">
      Raise {betValue}</button>
    <button
      type="button"
      onClick={() => fold()}
      className="button">
      Fold
    </button>
    <input
      type="text"
      value={betValue}
      onChange={handleChange}
    />
    <button
      type="button"
      onClick={() => sitDown(betValue)}
      className="button">
      SitDown {betValue}
    </button>
    <button
      type="button"
      onClick={() => leaveGameSeat()}
      className="button">
      LeaveGame
    </button>
  </div>)


export default ActionPanel
