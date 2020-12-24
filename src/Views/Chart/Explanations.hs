{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Views.Chart.Explanations where

import CMark ( commonmarkToHtml, optUnsafe, optSmart)
import Data.String.Interpolate.IsString ( i )
import Import (Bool, (&), HashMap,  ($), Monoid(mempty), (.), Text, forM_ )
import Lucid ( Html, ToHtml(toHtml, toHtmlRaw), dd_, dl_, dt_ )
import Ephemeris
    ( Planet(Chiron, Sun, Moon, Mercury, Venus, Mars, Jupiter, Saturn,
             Uranus, Neptune, Pluto, MeanNode, MeanApog),
      ZodiacSignName(..),
      HouseNumber(..),
      AspectName(..),
      House(..),
      PlanetPosition(..))
import Data.HashMap.Strict (member, empty, fromList, lookupDefault)

markdownToHtml :: Text -> Html ()
markdownToHtml = toHtmlRaw . commonmarkToHtml [optUnsafe, optSmart]

m_ :: Text -> Html ()
m_ = markdownToHtml

class Explicable factor where
  explain :: factor -> Html ()
  explain _ = mempty

  explanationAttributes :: factor -> HashMap Text (Html ())
  explanationAttributes _ = empty

  explanationAttribute :: factor -> Text -> (Html ())
  explanationAttribute f attr =
    lookupDefault mempty attr (explanationAttributes f)

  hasAttribute :: factor -> Text -> Bool
  hasAttribute f attr =
    attr `member` (explanationAttributes f)

instance Explicable House where
  explain = explain . houseNumber
  explanationAttributes = explanationAttributes . houseNumber
  explanationAttribute  = explanationAttribute . houseNumber
  hasAttribute = hasAttribute . houseNumber

instance Explicable PlanetPosition where
  explain = explain . planetName
  explanationAttributes = explanationAttributes . planetName
  explanationAttribute  = explanationAttribute . planetName
  hasAttribute = hasAttribute . planetName

instance Explicable HouseNumber  where
  explain I =
    markdownToHtml
      [i|
  The first house starting point is the **Ascendant**: where the Sun rose for the astrological event in the chart,
  and as such it is significant in understanding how an individual approaches life and
  is seen by others.
  |]
  

  explain IV =
    markdownToHtml
      [i|
  The fourth house's starting point is the **Immum Coeli (IC)**, which is the lowest point in the sky
  (the Sun's Nadir,) and as such it symbolizes the depths of one's connection to the soul. 
  |]


  explain VII =
    markdownToHtml
      [i|
  The seventh house's starting point is the **Descendant (DC)**, which is where the Sun sets,
  and marks the threshold between the inner and the outer worlds and qualities and as such
  indicates the kinds of relationships the individual seeks, and the kind of people she/he
  attracts.
  |]

  explain X =
    markdownToHtml
      [i|
  The tenth house's starting point is the **Midheaven (Medium Coeli, MC)**, which is the highest point in the
  sky (the Sun's zenith,) and as such symbolizes the culmination/goal of an individual's life.
  Where the fourth house (its opposite) symbolizes _roots_, the tenth house symbolizes _fruits_.
  |]

  -- | No special explanations for the other houses, only the axes.
  explain _ = mempty

  explanationAttributes I =
    [
      ("Keywords", "conscious self, identity, self expression, first impressions, appearance.")
    , ("Quadrant", "South-Eastern")
    , ("Alias", "House of the Self")
    , ("LongitudeHemisphere", m_ "**Eastern Hemisphere**: self-expression")
    , ("LatitudeHemisphere", m_ "**Southern Hemisphere**: inner-world, unconscious")
    ] & fromList

  explanationAttributes II =
    [
      ("Keywords", "material situation, finances, assets, property, self worth, moral values, stability, security.")
    , ("Quadrant", "South-Eastern")
    , ("Alias", "House of Value, Finances or Money")
    , ("LongitudeHemisphere", m_ "**Eastern Hemisphere**: self-expression")
    , ("LatitudeHemisphere", m_ "**Southern Hemisphere**: inner-world, unconscious")
    ] & fromList

  explanationAttributes III =
    [
      ("Keywords", "communication, perception, education, immediate environment (neighbors, family).")
    , ("Quadrant", "South-Eastern")
    , ("Alias", "House of Intellect")
    , ("LongitudeHemisphere", m_ "**Eastern Hemisphere**: self-expression")
    , ("LatitudeHemisphere", m_ "**Southern Hemisphere**: inner-world, unconscious")
    ] & fromList

  explanationAttributes IV =
    [
      ("Keywords", "home, family, ancestry, parents, childhood.")
    , ("Quadrant", "South-Western")
    , ("Alias", "House of Origins")
    , ("LongitudeHemisphere", m_ "**Western Hemisphere**: relations to others")
    , ("LatitudeHemisphere", m_ "**Southern Hemisphere**: inner-world, unconscious")
    ] & fromList

  explanationAttributes V =
    [
      ("Keywords", "inner child, creativity, hobbies, interests, entertainment.")
    , ("Quadrant", "South-Western")
    , ("Alias", "House of Pleasure")
    , ("LongitudeHemisphere", m_ "**Western Hemisphere**: relations to others")
    , ("LatitudeHemisphere", m_ "**Southern Hemisphere**: inner-world, unconscious")
    ] & fromList

  explanationAttributes VI =
    [
      ("Keywords", "health, body, wellbeing, work (vs. career,) obligations, pets, colleagues.")
    , ("Quadrant", "South-Western")
    , ("Alias", "House of Service")
    , ("LongitudeHemisphere", m_ "**Western Hemisphere**: relations to others")
    , ("LatitudeHemisphere", m_ "**Southern Hemisphere**: inner-world, unconscious")
    ] & fromList

  explanationAttributes VII =
    [
      ("Keywords", "one-to-one relationships, romantic partners, open enemies.")
    , ("Quadrant", "North-Western")
    , ("Alias", "House of Relationships")
    , ("LongitudeHemisphere", m_ "**Western Hemisphere**: relations to others")
    , ("LatitudeHemisphere", m_ "**Northern Hemisphere**: outer world, conscious")
    ] & fromList

  explanationAttributes VIII =
    [
      ("Keywords", "change, death, rebirth, crisis, sexuality, personal growth, shared assets, shared values.")
    , ("Quadrant", "North-Western")
    , ("Alias", "House of Transformation")
    , ("LongitudeHemisphere", m_ "**Western Hemisphere**: relations to others")
    , ("LatitudeHemisphere", m_ "**Northern Hemisphere**: outer world, conscious")
    ] & fromList

  explanationAttributes IX =
    [
      ("Keywords", "belief systems, ideologies, religion, higher learning, worldview.")
    , ("Quadrant", "North-Western")
    , ("Alias", "House of Spirituality")
    , ("LongitudeHemisphere", m_ "**Western Hemisphere**: relations to others")
    , ("LatitudeHemisphere", m_ "**Northern Hemisphere**: outer world, conscious")
    ] & fromList

  explanationAttributes X =
    [
      ("Keywords", "vocation, career (vs. work,) ambitions, achievements, social standing, social recognition.")
    , ("Quadrant", "North-Eastern")
    , ("Alias", "House of Ambition")
    , ("LongitudeHemisphere", m_ "**Eastern Hemisphere**: self-expression")
    , ("LatitudeHemisphere", m_ "**Northern Hemisphere**: outer world, conscious")
    ] & fromList

  explanationAttributes XI =
    [
      ("Keywords", "social life, groups, communities, feeling of belonging, contributions to the collective, encouragement from others.")
    , ("Quadrant", "North-Eastern")
    , ("Alias", "House of Friendships")
    , ("LongitudeHemisphere", m_ "**Eastern Hemisphere**: self-expression")
    , ("LatitudeHemisphere", m_ "**Northern Hemisphere**: outer world, conscious")
    ] & fromList

  explanationAttributes XII =
    [
      ("Keywords", "dreams, hidden strengths/weaknessess, karma, secret enemies, withdrawal, transcendence, recuperation, mystical experience.")
    , ("Quadrant", "North-Eastern")
    , ("Alias", "House of Secrets")
    , ("LongitudeHemisphere", m_ "**Eastern Hemisphere**: self-expression")
    , ("LatitudeHemisphere", m_ "**Northern Hemisphere**: outer world, conscious")
    ] & fromList


instance Explicable ZodiacSignName  where
  explanationAttributes Aries =
    fromList
      [ ("Element", "Fire"),
        ("Quality", "Cardinal"),
        ("Ruler", "Mars"),
        ("Motto", "I am"),
        ("Related house", "Self (First House)"),
        ("Strengths", "brave, direct, fearless, independent, leader."),
        ("Weaknesses", "aggressive, self-centered, pushy, inconsistent.")
      ]
  
  explanationAttributes Taurus =
    fromList
      [ ("Element", "Earth"),
        ("Quality", "Fixed"),
        ("Ruler", "Venus"),
        ("Motto", "I have"),
        ("Related house", "Value (Second house)"),
        ("Strengths", "steady, driven, tenacious, patient, persistent"),
        ("Weaknesses", "materialistic, stubborn, possessive, indulgent")
      ]
  explanationAttributes Gemini =
    fromList
      [ ("Element", "Air"),
        ("Quality", "Mutable"),
        ("Ruler", "Mercury"),
        ("Motto", "I think"),
        ("Related house", "Intellect (Third house)"),
        ("Strengths", "intelligent, adaptable, agile, informative"),
        ("Weaknesses", "exaggerating, deceptive, cunning, superficial")
      ]
  explanationAttributes Cancer =
    fromList
      [ ("Element", "Water"),
        ("Quality", "Cardinal"),
        ("Ruler", "Moon"),
        ("Motto", "I feel"),
        ("Related house", "Origins (Fourth house)"),
        ("Strengths", "nurturing, supportive, compassionate"),
        ("Weaknesses", "dependent, passive aggressive, moody")
      ]
  explanationAttributes Leo =
    fromList
      [ ("Element", "Fire"),
        ("Quality", "Fixed"),
        ("Ruler", "Sun"),
        ("Motto", "I will"),
        ("Related house", "Pleasure (Fifth house)"),
        ("Strengths", "brave, playful, warm, generous, charismatic"),
        ("Weaknesses", "egotistical, domineering, vain, controlling")
      ]
  explanationAttributes Virgo =
    fromList
      [ ("Element", "Earth"),
        ("Quality", "Mutable"),
        ("Ruler", "Mercury"),
        ("Motto", "I analyze"),
        ("Related house", "Service (Sixth house)"),
        ("Strengths", "modest, humble, orderly, altruistic, logical"),
        ("Weaknesses", "obsessive, overly critical, perfectionist")
      ]
  explanationAttributes Libra =
    fromList
      [ ("Element", "Air"),
        ("Quality", "Cardinal"),
        ("Ruler", "Venus"),
        ("Motto", "I balance"),
        ("Related house", "Relationships (Seventh house)"),
        ("Strengths", "charming, diplomatic, easy-going, polished"),
        ("Weaknesses", "indecisive, gullible, hypocritical, shallow")
      ]
  explanationAttributes Scorpio =
    fromList
      [ ("Element", "Water"),
        ("Quality", "Fixed"),
        ("Ruler", "Pluto (traditionally, Mars)"),
        ("Motto", "I desire"),
        ("Related house", "Transformation (Eight house)"),
        ("Strengths", "passionate, perceptive, sacrificing, determined"),
        ("Weaknesses", "vindictive, paranoid, destructive, jealous")
      ]
  explanationAttributes Sagittarius =
    fromList
      [ ("Element", "Fire"),
        ("Quality", "Mutable"),
        ("Ruler", "Jupiter"),
        ("Motto", "I aim"),
        ("Related house", "Spirituality (Ninth house)"),
        ("Strengths", "optimistic, moral, enthusiastic, open-minded"),
        ("Weaknesses", "lazy, restless, irresponsible, tactless")
      ]
  explanationAttributes Capricorn =
    fromList 
      [ ("Element", "Earth"),
        ("Quality", "Cardinal"),
        ("Ruler", "Saturn"),
        ("Motto", "I use"),
        ("Related house", "Ambitions (Tenth house)"),
        ("Strengths", "disciplined, strategic, ambitious, responsible"),
        ("Weaknesses", "pessimistic, greedy, cynical, ruthless, rigid")
      ]
  explanationAttributes Aquarius =
    fromList 
      [ ("Element", "Air"),
        ("Quality", "Fixed"),
        ("Ruler", "Uranus (traditionally, Saturn)"),
        ("Motto", "I know"),
        ("Related house", "Friendships (Eleventh house)"),
        ("Strengths", "inventive, humanistic, friendly, reformative"),
        ("Weaknesses", "emotionally detached, impersonal, aloof")
      ]
  explanationAttributes Pisces =
    fromList 
      [ ("Element", "Water"),
        ("Quality", "Mutable"),
        ("Ruler", "Neptune (traditionally, Jupiter)"),
        ("Motto", "I believe"),
        ("Related house", "Secrets (Twelfth house)"),
        ("Strengths", "mystical, intuitive, creative, romantic, sensitive"),
        ("Weaknesses", "escapist, unrealistic, submissive, codependent")
      ]

instance Explicable Planet  where
  explanationAttributes Sun =
    fromList 
      [ ("Group", "Personal"),
        ("Rulership", "Leo"),
        ("Keywords", "ego, basic personality, conscious, vitality, stamina, life energy, will, personal identity")
      ]
  explanationAttributes Moon =
    fromList 
      [ ("Group", "Personal"),
        ("Rulership", "Cancer"),
        ("Keywords", "subconscious, emotions, instincts, habits, moods, maternity")
      ]
  explanationAttributes Mercury =
    fromList 
      [ ("Group", "Personal"),
        ("Rulership", "Gemini, Virgo"),
        ("Keywords", "mind, communication, intellect, reason, language, intelligence")
      ]
  explanationAttributes Venus =
    fromList 
      [ ("Group", "Personal"),
        ("Rulership", "Taurus, Libra"),
        ("Keywords", "attraction, love, beauty, harmony, artistic sensibility")
      ]
  explanationAttributes Mars =
    fromList 
      [ ("Group", "Personal"),
        ("Rulership", "Aries"),
        ("Keywords", "aggression, drive, action, desire, competition, courage, passion, self-assertion")
      ]
  explanationAttributes Jupiter =
    fromList 
      [ ("Group", "Social"),
        ("Rulership", "Sagittarius"),
        ("Keywords", "luck, growth, expansion, optimism, abundance, faith")
      ]
  explanationAttributes Saturn =
    fromList 
      [ ("Group", "Social"),
        ("Rulership", "Capricorn"),
        ("Keywords", "structure, restriction, discipline, responsibility, obligation, concentration, limitation, material form")
      ]
  explanationAttributes Uranus =
    fromList 
      [ ("Group", "Spiritual"),
        ("Rulership", "Aquarius"),
        ("Keywords", "eccentricity, rebellion, reformation, unpredictable change, liberation, disruption")
      ]
  explanationAttributes Neptune =
    fromList 
      [ ("Group", "Spiritual"),
        ("Rulership", "Pisces"),
        ("Keywords", "dreams, intution, mysticism, imagination, delusion, disintegration of limits")
      ]
  explanationAttributes Pluto =
    fromList 
      [ ("Group", "Spiritual"),
        ("Rulership", "Scorpio"),
        ("Keywords", "transformation, power, death, rebirth, evolution")
      ]
  explanationAttributes MeanNode =
    fromList 
      [ ("Also known as", "Ascending Node, North Node"),
        ("Notes", "Some astrologers use the 'True Node': both values are calculated points in the Moon's orbit, and are only ever less than a degree apart."),
        ("Keywords", "future, life's direction, goals")
      ]
  explanationAttributes MeanApog =
    fromList 
      [ ("Also known as", "Black Moon Lilith, Mean apogee"),
        ("Notes", "Some astrologers calculate Lilith as a focal point in the Moon's orbit, we use the 'mean' value of the moon's apogee: the point in the moon's orbit farthest away from Earth."),
        ("Keywords", "repressed self, anguish, resentment, rejection")
      ]
  explanationAttributes Chiron =
    fromList 
      [ ("Keywords", "wounded self, healer, suffering, acceptance")
      ]
  explanationAttributes _ = empty

-- https://www.astro.com/astrowiki/en/Aspect
instance Explicable AspectName  where
  explain Conjunction =
    markdownToHtml
      [i|
> A conjunction represents a close bond and interpenetration of both energies. 
> The planets involved continually influence each other. 
> If these planetary energies are similar - for example Moon/Venus or Sun/Mars - the conjunction's potential is increased.
> In this case, the planets support each other and in combination often find it easier to assert themselves when facing others. 
> If two very different planets are close together - for example Mercury/Saturn or Mars/Neptune - the potential in each may end up being inhibited.

(copied from [Astrowiki](https://www.astro.com/astrowiki/en/Conjunction))
      |]
  explain Sextile =
    markdownToHtml
      [i|
> A key word for sextile is "excitement".
> The trine relationship between two planets is so easily expressed that it seems natural and obvious, whereas the sextile contains some of the tension of the 2-series (which includes the opposition and square.)
>
> As with the trine, the sextile usually indicates areas of life that go well for the person, but where opportunities can slip because the person may feel no pressure to develop his talents. The sextile represents a challenge to act, although the motivating factor is weaker than the analytical aspects in which the element of pressure is more prominent. When this happens the sextile merely functions as a weak trine. The sextile offers an opportunity to learn and expand one's horizons without the same degree of stress.

(copied from [Astrowiki](https://www.astro.com/astrowiki/en/Sextile))
      |]
  explain Square =
    markdownToHtml
      [i|
> Square aspects have an inherent potential for conflict between the planets involved, irrespective of whether their energies are of a similar quality or not, and they can have a tendency to hinder each other's development. This is the reason why astrologers in the past often referred to the square as an inhibiting or hard aspect. Square aspects will often manifest as pressure felt within the individual to "act out" according to the planets' natures.
> For example, someone with Mars square Uranus may have an unpredictable, explosive temper; as Mars rules anger and Uranus signifies sudden disruptive events.
>
> More constructively, squares will also show where the person might work energetically to manifest the planets' more positive natures, because the square is not an aspect of ease and relaxation.
> A square involving a 10th house planet, for example, might push the person to work very hard at his career.
>
> Modern astrology emphasises the special challenge of this aspect. The squares in a horoscope show where the native needs to work and what tests must be passed before maturing. This also means that squares serve as a constant reminder not to fall into lethargy or become too self-satisfied. In order to deal with squares in a positive way, it is necessary to be conscious of their inherent potential and to develop constructive expressions.

(copied from [Astrowiki](https://www.astro.com/astrowiki/en/Square))
      |]
  explain Trine =
    markdownToHtml
      [i|
> The trine is usually experienced as supportive and helpful. The flow of energy between the planets involved is unimpeded.
> The supportive energy of the trine comes naturally and does not need to be earned. The trine could therefore be considered a place of relative calm in the horoscope which does not have to be fought for.
> This is not only important for an individual's wellbeing, but is also important in that it enables the individual to concentrate her energies elsewhere. However, if trines dominate the horoscope, there is a danger that the individual may lack the motivation to work on his own development. Everything may seem so good that he sees little need to change anything.

(copied from [Astrowiki](https://www.astro.com/astrowiki/en/Trine))
      |]
  explain Opposition =
    markdownToHtml
      [i|
> The planets involved directly confront each other and the horoscope owner may tend to concentrate on the pole that appears to be more dominant to the detriment of the supposedly weaker one. If this happens the neglected energy is often experienced in the form of projections. It is also possible that the individual goes through alternating phases in which either one or the other pole is emphasised.
> The great challenge of the opposition is to integrate the two poles. The task is to transform the energy so that they strengthen each other and work together rather than work against each other. How easy this is likely to be will depend on the energies of the planets involved. If these energies are similar - for example an opposition between the Moon and Venus or the Sun and Mars - the task should be somewhat easier. However the task will be more difficult if they are very different - as would be the case with an opposition between Saturn and Mercury or Mars and Neptune.

(copied from [Astrowiki](https://www.astro.com/astrowiki/en/Opposition))
      |]
  explain SemiSextile =
    markdownToHtml
      [i|
> A semi-sextile normally connects two consecutive signs.
> A connection between two consecutive signs indicates a connection between two opposing energies, because consecutive signs always belong to different elements and have different qualities. For this reason semi-sextiles are often thought of as connecting two planets that have difficulty working together; or indicating difficulties or challenges because two energies that feel alien to one another need to be integrated.
>
> An alternative view is that the sequence of signs in the zodiac is not arbitrary but contains a deep logic. The energy inherent in each sign of the zodiac is a prerequisite for the following one. For this reason, two planets connected by a semi-sextile can work constructively together while retaining their clearly defined roles. The planet in the preceding sign paves the way, as it were, for the following one. This understanding can help to create opportunities out of the difficulties associated with semi-sextiles.

(copied from [Astrowiki](https://www.astro.com/astrowiki/en/Semi-Sextile))  
      |]
  explain Quincunx =
    markdownToHtml
      [i|
The quincunx is a "neutral aspect": it doesn't signify harmony or tension, but it does indicate a difficult to realize potential.

> A quincunx can involve an effort to reconcile two planets' meanings in the chart, but even when the initial signs seem positive, there is no guarantee of success. In the end, it may not prove possible to truly integrate the energies involved. For example, if Venus and Mars stand in quincunx to one another a person may try to boost her self-confidence by investing large amounts of energy (Mars) in order to achieve aesthetic goals (Venus). But she may feel that the energy invested never produces the desired results. For this reason the quincunx is thought to be difficult to deal with and is often referred to as an aspect of desire or longing. On the other hand it shows important themes which the horoscope owner must face. The lack of success can be an impetus to consider and possibly change established patterns of behaviour. In this sense the quincunx hints at new possibilities.
>
> These patterns of tension that are relatively minor, yet persistent and difficult to resolve lead to this aspect's second meaning: adjustment. It is often simply easier to live with a source of disappointment or aggravation than to take action to try to resolve it. Perhaps the person feels the present outcome is inevitable.
>
> Part of the problem is that it is difficult for a third planet, natal or by transit, to offer resolution. A third planet might sextile or trine one of the quincunx planets, yet square the other one. Resolution is best handled by identifying the basic needs of each planet and looking for non-stressful activities that draw on both their strengths.

(copied from [Astrowiki](https://www.astro.com/astrowiki/en/Quincunx))
      |]
  explain Quintile =
    markdownToHtml
      [i|
> The quintile points to talents and potential not necessarily indicated by the major aspects. This is particulary true of artistic talent. In comparison to trines, quintiles give a greater emphasis on the purposeful manifestation of one's talents, or making something of them; be they artistic, scientific, or demonstrations of one's personal power. Thus quintiles appear with some frequency in the charts of both artists and repressive political leaders. In each case they suggest talent plus the ambition to make something of it in the world.

(copied from [Astrowiki](https://www.astro.com/astrowiki/en/Quintile))
      |]
  explain BiQuintile =
    markdownToHtml
      [i|
> The biquintile is seen as especially helpful in artistic matters. In this respect it resembles the quintile. Individuals with such an aspect between two planets are often only vaguely aware of it. However, if someone is sensitive enough it can aid in developing creative powers which can help to find original solutions and throw a positive light on issues that initially appear problematic.

(copied from [Astrowiki](https://www.astro.com/astrowiki/en/Biquintile))
      |]
  explain SemiSquare =
    markdownToHtml
      [i|
> The energy of the semi-square is similar to that of the square and sesquisquare, being one of the 2-series of aspects. It is therefore counted as one of the analytical aspects which show tension but also challenges that can help us grow and learn. However, any conflicts felt are not as immediately apparent as they are in the case of the square, making it easier to avoid them and miss an opportunity for growth.
> So, a semi-square can indicate subtle tensions which we would rather ignore. This may intially seem tempting but is unlikely to lead to long term solutions.

(copied from [Astrowiki](https://www.astro.com/astrowiki/en/Semi-Square))
      |]
  explain Sesquisquare =
    markdownToHtml
      [i|
(Also known as _Sesquiquadrate_)

> The sesquiquadrate's energy is similar to that of the square and semi-square. It belongs to the group of analytical aspects divisible by 2, which create tension and challenge us to work and learn. But the conflict is less apparent than with the square. It is easier to supress, making it more difficult to learn from the challenge. Sesquiquadrates tend to indicate a smouldering conflict that one would prefer to ignore. Any relief would then tend to be short-lived. Sesquiquadrates therefore tend to indicate themes of a long-term and stubborn nature. The German astrologer Thomas Ring talks of a "tear in the structure".
>
> Sesquisquares can occur between planets that are either in the same element (such as 6 degrees Leo and 21 degrees Sagittarius) or in different elements. Theoretically the same-element sesquisquares should be easier to manage because the signs have the same basic approach to life.

(copied from [Astrowiki](https://www.astro.com/astrowiki/en/Sesquisquare))
      |]
  explain Septile =
    markdownToHtml
      [i|
> The number seven has long been associated with spirituality; an association born out by astrological investigation into the septile series. It is prominent in the horoscopes of members of the clergy, for example. A key word for the septile is "inspiration," be it religious or artistic. The septile indicates a potential to experience the energies of the planets concerned on a deeper spiritual level. For example, a septile between Venus and Jupiter may lead an individual to explore and expand his musical creativity. The energy of the septile is very subtle, and perhaps most people do not experience it in their ordinary lives, except during moments of wonder.

(copied from [Astrowiki](https://www.astro.com/astrowiki/en/Septile))
      |]
  explain Novile =
    markdownToHtml
      [i|
> According to Dane Rudhyar, the novile is a symbol for "the process of pregnancy which enables an idea or form to become organically viable".
>
> A novile aspect can signal the end of a particular phase or process and the beginning of a new one. However, a great deal of sensitivity is needed to be aware of this subtle energy.

(copied from [Astrowiki](https://www.astro.com/astrowiki/en/Nonagon))
      |]

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
represent the forces that express themselves in that area of life (in our practice, we 
see these as both _external_ and _internal_ forces, in the form of archetypes: universal symbols and
myths that human beings manifest.)

Different astrological traditions use different [Systems](https://www.astro.com/astrowiki/en/House_System)
to calculate where each of the houses starts (its _cusp_) . Western astrology has favored the
[Placidus](https://www.astro.com/astrowiki/en/Placidus_House_System) house system, and we use that system here.

House systems usually depend on the "quadrants" (as defined by the ascendant, IC, descendant and midheaven,)
and these, in turn, are relative to time and place: days and nights are seldom of equal length! In certain
locations, at certain times, the Placidus system may fail (usually when very close to the poles, where
the mathematical definition of the system doesn't hold anymore,) in these rare cases, we automatically
use the much simpler (and older!) [Porphyry](https://www.astro.com/astrowiki/en/House_System#Porphyry) system.

  |]

-- https://www.astro.com/astrowiki/en/Zodiac_Sign
generalSignsExplanation :: Html ()
generalSignsExplanation =
  markdownToHtml
    [i|
In Astrology, the zodiac signs represent the "celestial" facets of the individual (or event)
for whom the chart has been drawn: they are the personal characteristics or traits that, 
combined with the houses and planets present in the sign, manifest in an individual's personality.

Each Zodiac Sign has a _ruler_ (or two): a planet with which it has an affinity. If an individual's chart
shows a sign's ruler located in that sign, it is said to be at home and the planet's characteristics/influences
are said to be felt more strongly. 

Each Zodiac Sign also has an _affinity_ with a particular house; they share characteristics but the house's cusp
_may not be_ in its "related" sign for a given chart. 

Traditionally, signs are assigned one of the four classical [**elements**](https://www.astro.com/astrowiki/en/Element) (Earth, Air, Fire, Water.) This is known as its
_triplicity_. More recently, signs are related to a particular [**quality**](https://www.astro.com/astrowiki/en/Quality): this is known as its _quadruplicity_.

The combination of a sign's _element_ and _quality_ can be used to deduce some useful symbolism:

* <strong class="text-fire">Fire</strong> signs are said to be creative, passionate, spontaneous and forceful.
* <strong class="text-earth">Earth</strong> signs are said to be practical, dependable, builders, cautious and reliable.
* <strong class="text-air">Air</strong> signs are said to be communicative, clever, curious, objective.
* <strong class="text-water">Water</strong> signs are said to be intuitive, sensitive, compassionate, moody

Meanwhile,

* <strong class="text-light">Cardinal</strong> signs mark the _beginning_ of a season, and as such are initiators, ambitious, driven, enthusiastic.
* <strong class="text-light">Fixed</strong> signs are the _middle_ of a season, and as such are steady, stable, resolute, reliable
* <strong class="text-light">Mutable</strong> signs are the _end_ of a season, and as such are adaptable, flexible, resourceful, versatile
    |]

--
generalPlanetsExplanation :: Html ()
generalPlanetsExplanation =
  markdownToHtml
    [i|
The astrological concept of a _Planet_ differs quite a bit from the modern Astronomical concept: the Sun (a star,)
the Moon (a satellite,) Pluto (a dwarf planet,) the North and South Nodes, as well as Lilith (points in the Moon's orbit,)
and Chiron (an asteroid,) are often drawn in natal charts but are most definitely not what an astronomer would call a Planet:
as far as Astrology is concerned, the symbolic notion of a planet is a point in the skies that has a certain periodic behavior
that can be related, holistically, with cyclical/recurring patterns in the human psyche. As such, some astrologers use other
calculated points, fictional/derived bodies, and asteroids in their practice, if they believe they symbolize something of note.

Another difference from Astronomical practice, is that in Astrology planetary positions are **geocentric**: we talk about
where a planet was at any given time _as seen from the center of the Earth_, which not only means that the positions
are relative to Earth, but also subject to the distorting effects that Earth's gravity may have on light coming from these bodies!

As far as our astrological practice is concerned, we see planets as **archetypes**: forces within the human psyche that manifest
in combination with personality traits (as colored by the Zodiac Signs,) in various areas of life (as symbolized by the Houses.)

Additionally, planets may **aspect** each other, in which case their archetypes are said to influence each other in various ways.

Planets are sometimes grouped into three groups: **Personal** planets, which express their qualities more strongly in
the individual; **Social** planets, which have effects on the individual but also describe a society, and **Spiritual**
planets describe aspects that go beyond the personal and social, invisible aspects in the "collective unconscious."

Most traditional planets are said to be the **rulers** of one or two signs: when they're "at home," their influence is strongest.
    |]

generalAspectsExplanation :: Html ()
generalAspectsExplanation =
  markdownToHtml
    [i|
When drawing a chart, we locate planets and house cusps at certain _longitudes_, or positions, along the
ecliptic (which is a circular representation of the heavens as seen from Earth,) and as such they
can form angles with each other. In astrological practice, certain angles between planets or special points
like the _Ascendant_ or _Midheaven_ symbolize an interaction between the archetypes these points represent;
these angles are the **Aspects**. The angle may not be exact, for example, the Sun may be 92 degrees away from
Mars, and an astrologer may choose to still call that a square (which is an angle of 90 degrees.) 
The deviation from exact angle that's allowed to still recognize an aspect is called an _orb_. 

Different astrological practices have different orbs: sometimes bigger celestial bodies (as seen from Earth,) like the Sun or Moon 
are given larger orbs, or bodies that are close to each other are allowed wider orbs (like the Sun with Mercury;)
while certain points that don't cast any light (like the Mean Node or the Ascendant) are not allowed any orbs.
In our practice, [we use fairly generous orbs uniformly](#orbs-used), but we always show you the orb of any 
aspects, as well as precise positions for all celestial bodies, so you can always choose to ignore aspects
or add others if you believe they improve the psychological portrait of your chart!

Aspects are usually divided between **Major** aspects, which are said to have the most impact on an individual,
and **minor** aspects. Moreover, some aspects are said to be **harmonious** (or **synthetic**,) in which
case the two bodies involved are said to work in concert, while other aspects are known as **disharmonious** 
(or, more accurately, **analytical/challenging**) in which the energies are at odds and may prove challenging
-- but modern astrology sees this as an opportunity for growth and development, learning how to reconcile these
opposing forces, as opposed to "bad" or "fateful".
    |]

generalTransitsExplanation :: Html ()
generalTransitsExplanation =
  markdownToHtml
    [i|
[Transits](https://www.astro.com/astrowiki/en/Transit)
are aspects between the position of planets at a given time, and the position of planets
or axes in a natal chart. When a transiting planet forms an _exact_ aspect, the aspect is considered
**triggered**, or activated. 

What transits are relevant is definitely an art, and up to an astrologer's criteria: in our aspects table,
we show all aspects formed by transiting planets, using the same orbs as natal charts,
but only consider a transit to be **active** when it is _very_ close (less than one degree) away
from exactitude at the moment of querying. This is why the list of _active transit aspects_ appears much
smaller than all transit aspects shown in the summary table. We only draw active aspects, too.

The _period of activity_ is also a bit of an art: a lot of astrologers consider the **Applying** phase
(when a transiting planet is _approaching_ the transited point) to be more important than the
**Separating** phase (when a transiting planet is moving away,) and as such they consider the activity
to begin when the transiting planet is more than one degree away, but to end as soon as it's more
than few minutes past the transited point.
We currently take a naïve approach:
we simply consider a transit to begin its activity when it's at most one degree away from becoming, or
having become, exact, in the 24 hour period around the moment of query. To aid your own interpretation,
we show the letter `a` in the summary table when an aspect is in the applicative phase (about to become
exact), and an `s` when it's separating (moving away.) 

Given all of the above, the [tables of positions and aspect summary](#analyze) are the best starting point for your own
interpretation: we take a rather cautious approach to what aspects we consider active, but you can
draw your own conclusions based on the raw data and your personal situation!

If you have any suggestions about how we calculate or present transiting aspects,
please [leave us a note](https://github.com/lfborjas/freenatalchart.xyz/issues/new?assignees=&labels=transits&template=transit-or-aspect-suggestion.md&title=Transit+or+Aspect+Suggestion).
    |]
