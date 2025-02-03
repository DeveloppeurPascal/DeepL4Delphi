# Librairie cliente de l'API DeepL pour Delphi

[This page in English.](README.md)

[DeepL](https://www.deepl.com/) est un outil de traduction de textes et documents en ligne, également disponible sous forme de logiciel et d'APIs.

Ce projet est une librairie cliente en Pascal pour Delphi d'utilisation de l'API principale de traduction. Des exemples d'utilisation sont également proposés.

Pour utiliser l'[API de DeepL](https://www.deepl.com/pro-api) vous devez avoir un compte gratuit ou payant.

Ce dépôt de code contient un projet développé en langage Pascal Objet sous Delphi. Vous ne savez pas ce qu'est Dephi ni où le télécharger ? Vous en saurez plus [sur ce site web](https://delphi-resources.developpeur-pascal.fr/).

## Présentations et conférences

### Présentations en ligne

* [Internationaliser les projets VCL / FMX](https://serialstreameur.fr/webinaire-20200625.php) (en français)

### DelphiCon 2023

* [Automatically Translate Your Programs With DeepL API](https://serialstreameur.fr/automatically-translate-your-programs-with-deepl-api.html) (en anglais)
* [Traduire automatiquement ses logiciels avec DeepL et WebBroker](https://serialstreameur.fr/traduire-automatiquement-ses-logiciels-avec-deepl-et-webbroker.html) (en français)

### Twitch

Suivez mes streams de développement de logiciels, jeux vidéo, applications mobiles et sites web sur [ma chaîne Twitch](https://www.twitch.tv/patrickpremartin) ou en rediffusion sur [Serial Streameur](https://serialstreameur.fr/) la plupart du temps en français.

* [Sortie de Delphi 10.4.2 Sydney puis codage d'une librairie pour utiliser l'API De traduction de DeepL](https://serialstreameur.fr/live-coding-20210225.php) (en français, 25/02/2021)
* [Codage en Delphi autour de DeepL et des services web](https://serialstreameur.fr/live-coding-20220121.php) (en français, 21/01/2022)

## Langues prises en charge par l'API de traduction

Consultez [cette page](https://www.deepl.com/docs-api/translating-text/) de la documentation de l'API de traduction pour connaître la liste des langues supportées et leurs variantes.

## Installation des codes sources

Pour télécharger ce dépôt de code il est recommandé de passer par "git" mais vous pouvez aussi télécharger un ZIP directement depuis [son dépôt GitHub](https://github.com/DeveloppeurPascal/DeepL4Delphi).

La documentation technique du projet, générée avec [DocInsight](https://devjetsoftware.com/products/documentation-insight/), est disponible dans le dossier ./docs et sur [GitHub Pages](https://developpeurpascal.github.io/DeepL4Delphi). D'autres informations et des liens connexes sont disponibles sur [le site web du projet](https://deepl4delphi.developpeur-pascal.fr).

Si vous avez besoin d'explications ou d'aide pour utiliser ce projet dans les vôtres, n'hésitez pas à [me contacter](https://developpeur-pascal.fr/nous-contacter.php). Je pourrai soit vous orienter vers une ressource en ligne, soit vous proposer une assistance sous forme de prestation ou gratuite selon les cas. Vous pouvez aussi me faire signe à l'occasion d'une conférence ou d'une présentation en ligne.

## Compatibilité

En tant que [MVP Embarcadero](https://www.embarcadero.com/resources/partners/mvp-directory) je bénéficie dès qu'elles sortent des dernières versions de [Delphi](https://www.embarcadero.com/products/delphi) et [C++ Builder](https://www.embarcadero.com/products/cbuilder) dans [RAD Studio](https://www.embarcadero.com/products/rad-studio). C'est donc dans ces versions que je travaille.

Normalement mes librairies et composants doivent aussi fonctionner au moins sur la version en cours de [Delphi Community Edition](https://www.embarcadero.com/products/delphi/starter).

Aucune garantie de compatibilité avec des versions antérieures n'est fournie même si je m'efforce de faire du code propre et ne pas trop utiliser les nouvelles façons d'écrire dedans (type inference, inline var et multilines strings).

Si vous détectez des anomalies sur des versions antérieures n'hésitez pas à [les rapporter](https://github.com/DeveloppeurPascal/DeepL4Delphi/issues) pour que je teste et tente de corriger ou fournir un contournement.

## Licence d'utilisation de ce dépôt de code et de son contenu

Ces codes sources sont distribués sous licence [AGPL 3.0 ou ultérieure](https://choosealicense.com/licenses/agpl-3.0/).

Vous êtes libre d'utiliser le contenu de ce dépôt de code n'importe où à condition :
* d'en faire mention dans vos projets
* de diffuser les modifications apportées aux fichiers fournis dans ce projet sous licence AGPL (en y laissant les mentions de copyright d'origine (auteur, lien vers ce dépôt, licence) obligatoirement complétées par les vôtres)
* de diffuser les codes sources de vos créations sous licence AGPL

Si cette licence ne convient pas à vos besoins (notamment pour un projet commercial) je propose aussi [des licences classiques pour les développeurs et les entreprises](https://deepl4delphi.developpeur-pascal.fr).

Certains éléments inclus dans ce dépôt peuvent dépendre de droits d'utilisation de tiers (images, sons, ...). Ils ne sont pas réutilisables dans vos projets sauf mention contraire.

Les codes sources de ce dépôt de code comme leur éventuelle version compilée sont fournis en l'état sans garantie d'aucune sorte.

## Comment demander une nouvelle fonctionnalité, signaler un bogue ou une faille de sécurité ?

Si vous voulez une réponse du propriétaire de ce dépôt la meilleure façon de procéder pour demander une nouvelle fonctionnalité ou signaler une anomalie est d'aller sur [le dépôt de code sur GitHub](https://github.com/DeveloppeurPascal/DeepL4Delphi) et [d'ouvrir un ticket](https://github.com/DeveloppeurPascal/DeepL4Delphi/issues).

Si vous avez trouvé une faille de sécurité n'en parlez pas en public avant qu'un correctif n'ait été déployé ou soit disponible. [Contactez l'auteur du dépôt en privé](https://developpeur-pascal.fr/nous-contacter.php) pour expliquer votre trouvaille.

Vous pouvez aussi cloner ce dépôt de code et participer à ses évolutions en soumettant vos modifications si vous le désirez. Lisez les explications dans le fichier [CONTRIBUTING.md](CONTRIBUTING.md).

## Supportez ce projet et son auteur

Si vous trouvez ce dépôt de code utile et voulez le montrer, merci de faire une donation [à son auteur](https://github.com/DeveloppeurPascal). Ca aidera à maintenir le projet (codes sources et binaires).

Vous pouvez utiliser l'un de ces services :

* [GitHub Sponsors](https://github.com/sponsors/DeveloppeurPascal)
* Ko-fi [en français](https://ko-fi.com/patrick_premartin_fr) ou [en anglais](https://ko-fi.com/patrick_premartin_en)
* [Patreon](https://www.patreon.com/patrickpremartin)
* [Liberapay](https://liberapay.com/PatrickPremartin)
* [Paypal](https://www.paypal.com/paypalme/patrickpremartin)

ou si vous parlez français vous pouvez [vous abonner à Zone Abo](https://zone-abo.fr/nos-abonnements.php) sur une base mensuelle ou annuelle et avoir en plus accès à de nombreuses ressources en ligne (vidéos et articles).
