import React from 'react';

const Seat = ({ playerName, chips, isTurnToAct, hasPocketCards, position }) => (
  <div className={`seat-${position}-container`}>
    {hasPocketCards ?
      <div className='hidden-pocket-cards' >
        <div className='card pocket-one' />
        <div className='card pocket-two' />
      </div> : ''}
    <div className={`seat-${position}`}>
      <h4>{playerName || 'Seat'}</h4>
      {playerName ? <h4>{chips}</h4> : ''}
    </div>
  </div>);

export default Seat;