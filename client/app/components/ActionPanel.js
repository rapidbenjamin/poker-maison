import React from 'react'

// TODO move to own component called pocket cards
import Card from './Card'

const getPocketCards = cards =>
  cards !== undefined && cards !== null ? cards.map(card => {
    const rank = card.get('rank')
    const suit = card.get('suit')

    return (<Card
      key={rank + suit}
      rank={rank}
      suit={suit}
    />)
  }) : ''




const ActionPanel = ({
  updateBetValue,
  betValue,
  bet,
  raise,
  call,
  fold,
  check,
  postSmallBlind,
  postBigBlind,
  sitDown,
  leaveGameSeat,
  userPocketCards,
  gameStage,
  sitIn,
  bigBlind,
  maxCurrBet,
  isTurnToAct,
  availableActions,
  userPlayer
}) => {
  console.log('available actions', availableActions)
  console.log(gameStage)
  const preDealActions =
    gameStage === "PreDeal" ? <React.Fragment>

      {availableActions.includes("PostBigBlind") ?
        <button
          type="button"
          onClick={() => postBigBlind()} className="button">
          Post Big Blind
      </button> : ''}

      {availableActions.includes("PostSmallBlind") ?
        <button
          type="button" onClick={() => postSmallBlind()} className="button">
          post Small Blind
      </button> : ' '}

      {userPlayer ? '' : <button
        type="button"
        onClick={() => sitDown(betValue)}
        className="button">
        Sit Down Bet <span className='monospaced-font-bold'>
          {betValue}</span>
      </button>}

      {userPlayer && (userPlayer.get("_playerState") === "SatOut") ? <button
        type="button"
        onClick={() => sitIn()}
        className="button">
        Sit In
          </button> : ''}

      {userPlayer ? <button
        type="button"
        onClick={() => leaveGameSeat()}
        className="button">
        Leave Seat
    </button> : ' '}

    </React.Fragment> : '';


  let minBet = maxCurrBet >= bigBlind ? 2 * maxCurrBet : bigBlind

  return (
    <div className='action-panel'>


      <div className='user-actions-container'>
        {(availableActions.includes("Bet") || !userPlayer || availableActions.includes("Raise")) ?
          <div className="slidecontainer">
            <input type="range"

              max={userPlayer ? userPlayer.get("_chips") : 2000}
              min={minBet}
              step={5}
              value={betValue}
              className="slider"
              id="myRange"

              onChange={updateBetValue} />
          </div> : ''}
        {preDealActions}
        {true ?   // gameStage !== 'Showdown' && gameStage !== 'PreDeal' && isTurnToAct ?
          <React.Fragment>

            {availableActions.includes("Check") ?
              <button type="button" onClick={() => check()} className="button">
                Check
       </button> : ''}

            {availableActions.includes("Call") ?
              <button type="button" onClick={() => call()} className="button">
                Call</button> : ''}

            {availableActions.includes("Bet") ? <button
              type="button"
              onClick={() => bet(betValue)} className="button">Bet <span className='monospaced-font-bold'>
                {betValue}</span></button> : ''}
            {availableActions.includes("Raise") ?
              <button
                type="button"
                onClick={() => raise(betValue)}
                className="button">
                Raise {betValue}</button> : ''}
            {availableActions.includes("Fold") ? <button
              type="button"
              onClick={() => fold()}
              className="button">
              Fold
          </button> : ''}
          </React.Fragment>
          : ''}

      </div>
    </div>)
}


export default ActionPanel
