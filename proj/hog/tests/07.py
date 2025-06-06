test = {
  'name': 'Question 7',
  'points': 2,
  'suites': [
    {
      'cases': [
        {
          'code': r"""
          >>> is_always_roll(always_roll_5)
          2dd55144519866b51ac70638e37b5388
          # locked
          """,
          'hidden': False,
          'locked': True,
          'multiline': False
        },
        {
          'code': r"""
          >>> is_always_roll(always_roll(3))
          2dd55144519866b51ac70638e37b5388
          # locked
          """,
          'hidden': False,
          'locked': True,
          'multiline': False
        },
        {
          'code': r"""
          >>> is_always_roll(catch_up)
          c40a7c3c767f2fb1f2f369dabf5379f5
          # locked
          """,
          'hidden': False,
          'locked': True,
          'multiline': False
        },
        {
          'code': r"""
          >>> def s(x, y):
          ...    if x == 0 and y == 0:
          ...        return 0
          ...    else:
          ...        return 1
          >>> is_always_roll(s)
          False
          """,
          'hidden': False,
          'locked': False,
          'multiline': False
        },
        {
          'code': r"""
          >>> def s(x, y):
          ...    if x == 60 and y == 0:
          ...        return 0
          ...    else:
          ...        return 1
          >>> is_always_roll(s)
          False
          """,
          'hidden': False,
          'locked': False,
          'multiline': False
        },
        {
          'code': r"""
          >>> def s(x, y):
          ...    if x == 0 and y == 60:
          ...        return 0
          ...    else:
          ...        return 1
          >>> is_always_roll(s)
          False
          """,
          'hidden': False,
          'locked': False,
          'multiline': False
        },
        {
          'code': r"""
          >>> def s(x, y):
          ...    if x == 60 and y == 60:
          ...        return 0
          ...    else:
          ...        return 1
          >>> is_always_roll(s)
          False
          """,
          'hidden': False,
          'locked': False,
          'multiline': False
        },
        {
          'code': r"""
          >>> def s(x, y):
          ...    if x == 99 and y == 99:
          ...        return 0
          ...    else:
          ...        return 1
          >>> is_always_roll(s)
          False
          """,
          'hidden': False,
          'locked': False,
          'multiline': False
        },
        {
          'code': r"""
          >>> def s(x, y):
          ...    if x == 150 and y == 125:
          ...        return 0
          ...    else:
          ...        return 1
          >>> is_always_roll(s, 200) # GOAL is not always 100!
          False
          """,
          'hidden': False,
          'locked': False,
          'multiline': False
        }
      ],
      'scored': True,
      'setup': r"""
      >>> from hog import *
      """,
      'teardown': '',
      'type': 'doctest'
    }
  ]
}
