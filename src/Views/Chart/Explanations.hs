{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Views.Chart.Explanations where

import CMark
import Data.String.Interpolate.IsString
import Import
import Lucid
import SwissEphemeris (ZodiacSignName (..))

markdownToHtml :: Text -> Html ()
markdownToHtml = toHtmlRaw . commonmarkToHtml []
--explainPlanet :: Planet -> Html ()
--explainAspect :: Aspect -> Html ()
class Explicable factor where
  explain :: factor -> Html ()

instance Explicable HouseNumber where
  explain I =
    markdownToHtml
      [i|
  The first house (_Ascendant_) is in the **South-Eastern** quadrant.
  The Southern (nocturnal) hemisphere symbolizes one's inner/unconscious qualities, while
  the Eastern hemisphere symbolizes how the Self relates to the world.
  
  The first house starting point is the **Ascendant**: where the Sun rose for the astrological event in the chart,
  and as such it is significant in understanding how an individual approaches life and
  is seen by others.

  It is often described as the House of the **Self**.

  **Keywords**: conscious self, identity, self expression, first impressions, appearance.
  |]
  explain II =
    markdownToHtml
      [i|
  The second house is in the **South-Eastern** quadrant.
  The Southern (nocturnal) hemisphere symbolizes one's inner/unconscious qualities, while
  the Eastern hemisphere symbolizes how the Self relates to the world.

  The second house is often named the house of **Value, Finances or Money**.

  **Keywords**: material situation, finances, assets, property, self worth, moral values, stability, security.
  |]
  explain III =
    markdownToHtml
      [i|
  The third house is in the **South-Eastern** quadrant.
  The Southern (nocturnal) hemisphere symbolizes one's inner/unconscious qualities, while
  the Eastern hemisphere symbolizes how the Self relates to the world.

  It is known as the house of **Intellect**.

  **Keywords**: communication, perception, education, immediate environment (neighbors, family).
  |]
  explain IV =
    markdownToHtml
      [i|
  The fourth house is in the **South-Western** quadrant.
  The Southern (nocturnal) hemisphere symbolizes one's inner/unconscious qualities, while
  the Western hemisphere symbolizes how others influence/relate to the self.

  The fourth house's starting point is the **Immum Coeli (IC)**, which is the lowest point in the sky
  (the Sun's Nadir,) and as such it symbolizes the depths of one's connection to the soul. 
  
  It's known as the house of **Origins**.

  **Keywords**: home, family, ancestry, parents, childhood.
  |]
  explain V =
    markdownToHtml
      [i|
  The fifth house is in the **South-Western** quadrant.
  The Southern (nocturnal) hemisphere symbolizes one's inner/unconscious qualities, while
  the Western hemisphere symbolizes how others influence/relate to the self.

  It's known as the house of **Pleasure**.

  **Keywords**: inner child, creativity, hobbies, interests, entertainment.
  |]
  explain VI =
    markdownToHtml
      [i|
  The sixth house is in the **South-Western** quadrant.
  The Southern (nocturnal) hemisphere symbolizes one's inner/unconscious qualities, while
  the Western hemisphere symbolizes how others influence/relate to the self.

  It's known as the house of **Service**.

  **Keywords**: health, body, wellbeing, work (vs. career,) obligations, pets, colleagues.
  |]
  explain VII =
    markdownToHtml
      [i|
  The seventh house is in the **North-Western** quadrant.
  The Northern (diurnal) hemisphere symbolizes one's extroverted/conscious qualities, while
  the Western hemisphere symbolizes how others influence/relate to the self.

  The seventh house's starting point is the **Descendant (DC)**, which is where the Sun sets,
  and marks the threshold between the inner and the outer worlds and qualities and as such
  indicates the kinds of relationships the individual seeks, and the kind of people she/he
  attracts.

  It's known as the house of **Relationships**. 
  
  **Keywords**: one-to-one relationships, romantic partners, open enemies.
  |]
  explain VIII =
    markdownToHtml
      [i|
  The eighth house is in the **North-Western** quadrant.
  The Northern (diurnal) hemisphere symbolizes one's extroverted/conscious qualities, while
  the Western hemisphere symbolizes how others influence/relate to the self.

  It's known as the house of **Transformation**.

  **Keywords**: change, death, rebirth, crisis, sexuality, personal growth, shared assets, shared values.
  |]
  explain IX =
    markdownToHtml
      [i|
  The ninth house is in the **North-Western** quadrant.
  The Northern (diurnal) hemisphere symbolizes one's extroverted/conscious qualities, while
  the Western hemisphere symbolizes how others influence/relate to the self.

  It's known as the house of **Spirituality**.

  **Keywords**: belief systems, ideologies, religion, higher learning, worldview.
  |]
  explain X =
    markdownToHtml
      [i|
  The tenth house is in the **North-Eastern** quadrant.
  The Northern (diurnal) hemisphere symbolizes one's extroverted/conscious qualities, while
  the Eastern hemisphere symbolizes how the Self relates to the world.

  The tenth house's starting point is the **Midheaven (Medium Coeli, MC)**, which is the highest point in the
  sky (the Sun's zenith,) and as such symbolizes the culmination/goal of an individual's life.
  Where the fourth house (its opposite) symbolizes _roots_, the tenth house symbolizes _fruits_.

  It's known as the house of **Ambition**.

  **Keywords**: vocation, career (vs. work,) ambitions, achievements, social standing, social recognition.
  |]
  explain XI =
    markdownToHtml
      [i|
  The eleventh house is in the **North-Eastern** quadrant.
  The Northern (diurnal) hemisphere symbolizes one's extroverted/conscious qualities, while
  the Eastern hemisphere symbolizes how the Self relates to the world.

  It's known as the house of **Friendships**.
  
  **Keywords**: social life, groups, communities, feeling of belonging, contributions to the collective, encouragement from others.
  |]
  explain XII =
    markdownToHtml
      [i|
  The twelfth house is in the **North-Eastern** quadrant.
  The Northern (diurnal) hemisphere symbolizes one's extroverted/conscious qualities, while
  the Eastern hemisphere symbolizes how the Self relates to the world.

  It's known as the house of **Secrets**.

  **Keywords**: dreams, hidden strengths/weaknessess, karma, secret enemies, withdrawal, transcendence, recuperation, mystical experience.
  |]

instance Explicable ZodiacSignName where
  explain Aries =
    descriptions
      [ ("Element", "Fire"),
        ("Quality", "Cardinal"),
        ("Ruler", "Mars"),
        ("Motto", "I am"),
        ("Related house", "Self (First House)"),
        ("Strengths", "brave, direct, fearless, independent, leader."),
        ("Weaknesses", "aggressive, self-centered, pushy, inconsistent.")
      ]
  explain Taurus =
    descriptions
      [ ("Element", "Earth"),
        ("Quality", "Fixed"),
        ("Ruler", "Venus"),
        ("Motto", "I have"),
        ("Related house", "Value (Second house)"),
        ("Strengths", "steady, driven, tenacious, patient, persistent"),
        ("Weaknesses", "materialistic, stubborn, possessive, indulgent")
      ]
  explain Gemini =
    descriptions
      [ ("Element", "Air"),
        ("Quality", "Mutable"),
        ("Ruler", "Mercury"),
        ("Motto", "I think"),
        ("Related house", "Intellect (Third house)"),
        ("Strengths", "intelligent, adaptable, agile, informative"),
        ("Weaknesses", "exaggerating, deceptive, cunning, superficial")
      ]
  explain Cancer =
    descriptions
      [ ("Element", "Water"),
        ("Quality", "Cardinal"),
        ("Ruler", "Moon"),
        ("Motto", "I feel"),
        ("Related house", "Origins (Fourth house)"),
        ("Strengths", "nurturing, supportive, compassionate"),
        ("Weaknesses", "dependent, passive aggressive, moody")
      ]
  explain Leo =
    descriptions
      [ ("Element", "Fire"),
        ("Quality", "Fixed"),
        ("Ruler", "Sun"),
        ("Motto", "I will"),
        ("Related house", "Pleasure (Fifth house)"),
        ("Strengths", "brave, playful, warm, generous, charismatic"),
        ("Weaknesses", "egotistical, domineering, vain, controlling")
      ]
  explain Virgo =
    descriptions
      [ ("Element", "Earth"),
        ("Quality", "Mutable"),
        ("Ruler", "Mercury"),
        ("Motto", "I analyze"),
        ("Related house", "Service (Sixth house)"),
        ("Strengths", "modest, humble, orderly, altruistic, logical"),
        ("Weaknesses", "obsessive, overly critical, perfectionist")
      ]
  explain Libra =
    descriptions
      [ ("Element", "Air"),
        ("Quality", "Cardinal"),
        ("Ruler", "Venus"),
        ("Motto", "I balance"),
        ("Related house", "Relationships (Seventh house)"),
        ("Strengths", "charming, diplomatic, easy-going, polished"),
        ("Weaknesses", "indecisive, gullible, hypocritical, shallow")
      ]
  explain Scorpio =
    descriptions
      [ ("Element", "Water"),
        ("Quality", "Fixed"),
        ("Ruler", "Pluto (traditionally, Mars)"),
        ("Motto", "I desire"),
        ("Related house", "Transformation (Eight house)"),
        ("Strengths", "passionate, perceptive, sacrificing, determined"),
        ("Weaknesses", "vindictive, paranoid, destructive, jealous")
      ]
  explain Sagittarius =
    descriptions
      [ ("Element", "Fire"),
        ("Quality", "Mutable"),
        ("Ruler", "Jupiter"),
        ("Motto", "I aim"),
        ("Related house", "Spirituality (Ninth house)"),
        ("Strengths", "optimistic, moral, enthusiastic, open-minded"),
        ("Weaknesses", "lazy, restless, irresponsible, tactless")
      ]
  explain Capricorn =
    descriptions
      [ ("Element", "Earth"),
        ("Quality", "Cardinal"),
        ("Ruler", "Saturn"),
        ("Motto", "I use"),
        ("Related house", "Ambitions (Tenth house)"),
        ("Strengths", "disciplined, strategic, ambitious, responsible"),
        ("Weaknesses", "pessimistic, greedy, cynical, ruthless, rigid")
      ]
  explain Aquarius =
    descriptions
      [ ("Element", "Air"),
        ("Quality", "Fixed"),
        ("Ruler", "Uranus (traditionally, Saturn)"),
        ("Motto", "I know"),
        ("Related house", "Friendships (Eleventh house)"),
        ("Strengths", "inventive, humanistic, friendly, reformative"),
        ("Weaknesses", "emotionally detached, impersonal, aloof")
      ]
  explain Pisces =
    descriptions
      [ ("Element", "Water"),
        ("Quality", "Mutable"),
        ("Ruler", "Neptune (traditionally, Jupiter)"),
        ("Motto", "I believe"),
        ("Related house", "Secrets (Twelfth house)"),
        ("Strengths", "mystical, intuitive, creative, romantic, sensitive"),
        ("Weaknesses", "escapist, unrealistic, submissive, codependent")
      ]

descriptions :: [(Text, Text)] -> Html ()
descriptions ds =
  dl_ $ do
    forM_ ds $ \(term', definition) -> do
      dt_ [] $ toHtml term'
      dd_ [] $ toHtml definition

attribution :: Html ()
attribution =
  markdownToHtml
    [i|
All explanations about astrological factors are based on information found in:

* An upcoming workbook by [Labyrinthos Academy](https://labyrinthos.co/)
* Articles in [Astrowiki](https://www.astro.com/astrowiki/en/Main_Page) by Astrodienst (modified copy.)

|]

-- https://www.astro.com/astrowiki/en/House
generalHousesExplanation :: Html ()
generalHousesExplanation =
  markdownToHtml
    [i|
In Astrology, the houses are 12 divisions of the heavens that represent the "earthly"
facets of the individual or event for whom the chart has been drawn: they are the
areas of life in which the innate forces of the **Planets**, and the characteristics
of the **Zodiac Signs**, find manifestation in how an individual lives their life. 


The beginning point of a house (known as the _cusp_,) may fall within a sign, and encompass
one or more zodiac signs until the cusp of the next house. These contained signs indicate how
the individual approaches the house's area of life. The planets contained in the house
represent the external forces that express themselves in that area of life.
  |]


-- https://www.astro.com/astrowiki/en/Zodiac_Sign
generalSignsExplanation :: Html ()
generalSignsExplanation =
  markdownToHtml
    [i|
In Astrology, the zodiac signs represent the "celestial" facets of the individual (or event)
for whom the chart has been drawn: they are the personal characteristics or traits that, 
combined with the houses and planets present in the sign, manifest in an individual's personality.

Each Zodiac Sign has a _ruler_ (or two,): a planet with which it has an affinity. If an individual's chart
shows a sign's ruler present in that house, it is said to be at home and the planet's characteristics/influences
are said to be felt more strongly. 

Each Zodiac Sign also has an _affinity_ with a particular house; they share characteristics but the house's cusp
_may not be_ in its "related" sign for a given chart. 
    |]
