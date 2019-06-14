import React from 'react'

const MyComponent = require('./Counter.bs').make

const Profile = username => (
  <div className="profile">
    profile <MyComponent name="Regina" />
  </div>
)

export default Profile
