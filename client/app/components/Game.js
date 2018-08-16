import React from 'react'

const Game = ({ username, isTurnToAct, game, call, bet, fold, raise, check, postBigBlind, postSmallBlind }) => {
  console.log('game', game)
  if (game) {
    const jsgame = game.toJS()
    jsgame._deck = undefined // hide deck

    return (<div>
      <p style={{ height: ' 500px' }}>{(JSON.stringify({ ...jsgame, isTurnToAct, username }, undefined, '\n'))}
      </p>
      <button type="button" onClick={() => postBigBlind()} className="button">postBigBlind</button>
      <button type="button" onClick={() => postSmallBlind()} className="button">postSmallBlind</button>
      <button type="button" onClick={() => check()} className="button">check</button>
      <button type="button" onClick={() => call()} className="button">call</button>
      <button type="button" onClick={() => bet(100)} className="button">Bet 100</button>
      <button type="button" onClick={() => raise(100)} className="button">Raise 100</button>
      <button type="button" onClick={() => fold()} className="button">Fold</button>
    </div>)
  }
  else {
    return <h2>no Game State</h2>
  }
}

export default Game