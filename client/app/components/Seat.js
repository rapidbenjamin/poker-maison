import React from 'react';

const Seat = ({ playerName, chips, isTurnToAct, hasPocketCards, position }) => (
  <div className={`seat-${position}-container`}>
    {hasPocketCards ?
      <div className='hidden-pocket-cards' >
        <div className='hidden-pocket-cards-container' >
          <div className='card pocket-one' />
          <div className='card pocket-two' />
        </div>
      </div> : ''}
    <div className={`seat-${position} ${isTurnToAct ? 'active-player' : ''} ${playerName ? '' : 'empty-seat'}`}>
      <h5 className={playerName ? 'player-name' : ''}>{playerName || 'Take Seat'}</h5>
      {playerName ? <h5 className='player-chip-count'><span className='monospaced-font'>
        {chips}</span></h5> : ''}
    </div>
  </div>);

export default Seat;