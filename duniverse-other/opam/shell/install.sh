#!/bin/sh

set -ue

# (c) Copyright Fabrice Le Fessant INRIA/OCamlPro 2013
# (c) Copyright Louis Gesbert OCamlPro 2014-2017

VERSION='2.0.8'
DEV_VERSION='2.1.0~rc2'
DEFAULT_BINDIR=/usr/local/bin

bin_sha512() {
  case "$OPAM_BIN" in
    opam-2.0.6-arm64-linux)     echo "d2b3d92fd5fae7f053702b53ddbc7c224fcfbfc9b232247ba4e40cbf1cda28f160d8c14fde87aebeebfd2545e13265c0ee4a47e292f035767fb944b1b8ff5c90";;
    opam-2.0.6-armhf-linux)     echo "a42a7ad8c1afdb20ac5746934306576e6364f5453b176ccd42a3e5a116a5db05c2758cec31800ffab11411290cf671f9eee3f299df48c7ceca8e4d7e33dfedc8";;
    opam-2.0.6-i686-linux)      echo "6c0d965f89a2026ead3120e217d12b2df7426740d54bc94e2c46faaeff5893081e68aac162621bfa694ab597a18be28165f10cdda1217a4d73653789a9928b64";;
    opam-2.0.6-x86_64-linux)    echo "2b9d4a99aa28a193c88c7c6f6265203bd3cfeef98929d6f5cfce4b52cd9ddbd7be7eddc1d3d9c440f81d65074dd7851b8d29cd397fb06d2cfccffb54d3cdcc6a";;
    opam-2.0.6-x86_64-macos)    echo "cf02546b22ca91b1d97a3657b970b34d4acf4dc745696b7200ff185d25ebb5914ea8b6a94b503eb8c999634de6fdb944998a970105cd6a4c6df538c262b48b7f";;
    opam-2.0.6-x86_64-openbsd)  echo "2f58b3d4902d4c3fb823d251a50e034f9101b0c5a3827725876bb3bcb6c013c4f54138054d82abba0a9e917675275e26f05b98630cf7116c465d2110756f1309";;

    opam-2.0.7-arm64-linux)     echo "0dd4d80496545f684af39dc5b4b28867bc19a74186577c38bd2a8934d871c2cbcdb9891bfd41c080b5f12d5a3c8801e203df8a76d55e1e22fe80d31447402e46";;
    opam-2.0.7-arm64-macos)     echo "ff8463f4450eff488d458be209e04cbc2286265b967b8cbcd9c7dd4b9bebad5951c0e4f42fc96a7745da7d80750eb1d30046ea2b5de41e3a78487f547229a8db";;
    opam-2.0.7-armhf-linux)     echo "ea691bc9565acc1207dea3dfb89192b1865b5b5809efe804a329f39878640fb19771edcb05c5699f8e914e88e3155f31132b845c54b0095bedd3952d336bae0b";;
    opam-2.0.7-i686-linux)      echo "5fa8fb9664d36ead5760e7e1c337f6ae7b0fd4be5089ddfb50ae74028deec30893b1f4dee040402bc3f15da197ba89a45c7d626ecf6e5be80d176f43526c4bad";;
    opam-2.0.7-x86_64-linux)    echo "da75b0cb5ad50f95d31857a7d72f1836132a1fa1cdbfdedf684342b798e7107b4add4c74c05d5ce44881309fa1e57707538dbcda874e7f74b269b1bb204f3ae3";;
    opam-2.0.7-x86_64-macos)    echo "de1194c8e97e53956e5e47502c28881bbf26d1beaac4f33a43a922b8ca7ce97725533cfaf65a33fc0e183eab5a95e9ecd2e20f72faeaec333dc3850b79b5fe8a";;
    opam-2.0.7-x86_64-openbsd)  echo "b253809c4388847e1a33b5c4f1f5d72bef79a2f0c43b19ef65b40d0c10341aa0bee4a4b1f3a9ab70eb026e4cc220a63cfc56a18c035b6b0297c92f2bdb7f9a78";;

    opam-2.0.8-arm64-linux)     echo "74f23cd478371e9f1de88bb89ab6d07985c2077b3392288399e37499514c03b7c826f29147282d147efeba0700aee73e8b8230ee77ba0a433dddfe42d15b1df7";;
    opam-2.0.8-arm64-macos)     echo "45eb0a414f66307a566974eda14dc6061b999e4fcb6626b39477bdbf189c75b1c0f20bd480d88d2c41ea77658073e2cbb5b73c969e187de7de311bbae516216e";;
    opam-2.0.8-armhf-linux)     echo "56cd91ee076e3eb54b18028e6551776fb0568b459292cdc78ebbffc9ca684454f525b9dcb37e46c7a191492221387ab9abdbedc7754d0fe6ab278e3ad548c614";;
    opam-2.0.8-i686-linux)      echo "ee7b6bac2c94c096580cbcc73ad9105ec62f1a899339d13f18bd8a0abc0d3a4daaa5dca580712744d4cc52d7b319a2274c427f18af709619e98f1cd3fe78fe0e";;
    opam-2.0.8-x86_64-linux)    echo "66eee43400fa014e2128a2ffb82b36f7a9bcec4c1d243c7a38bb608ab86c8e814a7b1219e6f21beedac28b2b8c51b233377a8f7bd22fee2ff3c9c56f9264f3f0";;
    opam-2.0.8-x86_64-macos)    echo "17b7f6716dd0540f2405474451de80c3badfe4de06b8b747e9b9302d8897433f4235aa1166e8bf7c873254f92f5a7087525435b65b62e1fe00fa57d01936b430";;
    opam-2.0.8-x86_64-openbsd)  echo "05203d65d391b92350390dff7a1ffaca2b67bba9ca948cea26607f0cf7f19f30070d63f4f3bd78aff1b1c062d764e4a938106d072d8aa66740fb28480feabac5";;

    opam-2.1.0-alpha-arm64-linux)     echo "1bf0acfa64aa01c3244e65eed60eef1caaa6de53aa8b32dd0d2446f91905a1e41591f53cd350e85b2b9f5edba9b137d723c32949115623e9753e77b707bb25b0";;
    opam-2.1.0-alpha-armhf-linux)     echo "87c12a422bd14a0d10a94ddaaa46de23700e3b89810a0c06232eff8d96b37c2fd43dcb5a8da5a2004aa8040d1b93293209f1ff1aab865ffd150364e24c87c716";;
    opam-2.1.0-alpha-i686-linux)      echo "b8369da6d4795a461ff1b49e687b027325d4e90bc8f19517e52a94ee3be167c4faaaf33bd0b3536be552d2add54865d0e33933acaa674f2e1a17249b022738af";;
    opam-2.1.0-alpha-x86_64-linux)    echo "2e22747829fb0bada3a74a23f5e0ff2228520d647fc4fe08a1ce76f3cb357cc7240f7b45e422c5f4b8eafe832ae3a8973ecbd4814ae0e8ce1096bcff39482020";;
    opam-2.1.0-alpha-x86_64-macos)    echo "c440e8ae1970fa7533e6e1b96ba3e3dd65b04432d41bc57ce4c768ed9b4229954546d59ec06f3d4ee49cbe00bb4bfd0b3f509d6d9a27de2db17725e097a61c86";;
    opam-2.1.0-alpha-x86_64-openbsd)  echo "d87afe99fee541a1c6fae30b72653db7a5ea2abdec3fa3b2b480daddf3fcd8d4096e2a40458310755faec3722119f29ed981ffbfa65142e618f99b70572f892f";;

    opam-2.1.0-alpha2-arm64-linux)    echo "b67520bb2a6c59f800da100278d74e58f2bbf66924f94643023dc46b97b16f17a30de95e439c6f9b032bd555c062ddba325f3e5169cac186615b959a8c434788";;
    opam-2.1.0-alpha2-armhf-linux)    echo "9a6312eb54d6c9c2036ca90f7816789c27c23f1b1d325cd69d27a910cdd8760b82f19c9e9b61b5b6214818f1f40f8b4d2ef081acb43f0dad68c976986a7c6a45";;
    opam-2.1.0-alpha2-i686-linux)     echo "0dc07f236405777ad74d58fcc6cb6c3247e7dfc31408df4a199599077d5cb41ec86895f1d0c5eaa2a9c70842a2a998226674f986ba0044c82896c073ac90b209";;
    opam-2.1.0-alpha2-x86_64-linux)   echo "21509e8abd8463f4e18a55398f690700772e25f0ddb9f3fd7644e2f9a9a89ebbf5c09efbeceafe4a0ab5015d0d03b2f29506be514aae813a2f3dac7dd01261f3";;
    opam-2.1.0-alpha2-x86_64-macos)   echo "1c1bd26621eebb5bf3783dec80d5555aa5ff02dcbf43eb44398798e6162c1964bc1964e3980391ea115e5c068c1bb66960f8ebdd91bc4f0bac844f3a61433f1e";;
    opam-2.1.0-alpha2-x86_64-openbsd) echo "941f3e306bc36e8e44e4245ca5e635b04e0a54f33439d55d41875ced47384cad8c222b649027d3c4eacc3c2c569cf5006c872763b19c490d9b289c9cfe4f491a";;

    opam-2.1.0-alpha3-arm64-linux)     echo "ad906bb2ab764a92fabdf0b906310c5034bf5daf0ebfb2529e9b87661ddbf8fd14f51dee5ce75b4fd4bb5789e29c7be71063f1ebcc92e92333be12aa62efdff9";;
    opam-2.1.0-alpha3-armhf-linux)     echo "2a7022c1f5dbc855a0d067f29677b13253dccbc9792b8170fa72a743802bbcd6e41ce7512c4845091af0f73b8ba7573038ec53ea9aaf74be04367ac1767e7220";;
    opam-2.1.0-alpha3-i686-linux)      echo "6f2fce0c45ae700e7a1b32d0a24988645c9aed3afc45998c8fbe70e97a65e3ba5d824069914a892bb3f9b1336383cfd492c28678ff16db5cada863da924b07d8";;
    opam-2.1.0-alpha3-x86_64-linux)    echo "1d219dbf670e1550bf71c28e586d14f1d8af2605f0e13bea2f11ad52a7f176bd9a89637e44a91a024f0088db1b2aba8dc3207bc81fa930580e54f4031255c178";;
    opam-2.1.0-alpha3-x86_64-macos)    echo "93edb6c1151f8f5bd017f230ffd9277f6ad943e3f5032ea000c37f012738fb3ab4b4add172e1f624c37e6564963fef0716b876b0113c8e43f5943d77bbbc173c";;
    opam-2.1.0-alpha3-x86_64-openbsd)  echo "0e3b3761e877c57f5b333aacb70c86bf60f50eecdca6e9e1a552e3d666cea034d8873f3a87e585a5970b1aef7e540adb18c71e0e8fd8794843dd5d1d421a87ec";;

    opam-2.1.0-beta-arm64-linux)     echo "954670c74ea8244b440756e4f7755bd2b5548ab67428ce577c4c507fc33c8d00eb73c4d7b59ccb0ef800f4465b5c704573c63486b78a23e9568f3751bf9aef78";;
    opam-2.1.0-beta-armhf-linux)     echo "cc666f2c6b1ac07d1bc8a035c6b3a9455794b51a827c54bb92786ae1a75c6c55839d3f48b378508f42a66ac887fdc68f7628a67e2826813cb6df048c906755ca";;
    opam-2.1.0-beta-i686-linux)      echo "66ac48b298741f753ca868be362851ccd9bf84fd8772d18f3307e99cf72c8c68ac9fa17bf2d610d7f3b5dc6209eb8371bf0e10b363e963fc6c31d70e5938017f";;
    opam-2.1.0-beta-x86_64-linux)    echo "e316f1b5f1c668affba6c2819f692c28776e131a17fb64b2c0e23f8a3b7d456575a8109fcdcb9babfad13bc33c17fa619cbb4a48ca6198765f86296b7e611f24";;
    opam-2.1.0-beta-x86_64-macos)    echo "acb29b7c64df314c6629e14f6d8f079504d39b7fd3104867fd22df3395ccfea9f1014a3a87dff9c12bf03ca451e9ee2918b9d9d8f17ce1a6d7de0c0649452fa9";;
    opam-2.1.0-beta-x86_64-openbsd)  echo "ff9fa1ee0ae7e54b4e18999cf5ea9b899c0b4039b744a950e96221e3e86c21eaa50904bdbc836ff8103f7713506d0de3d32ec77b169561e0cd694bfeea812cae";;

    opam-2.1.0-beta2-arm64-linux)     echo "a58ba3ebb4431d3cabfe96b806c9897205153e8a546ebe74f0229982758d140b4fcbcea421db70589b1eb3080dc86534522a3cba0330ce82e0898a60048d51ba";;
    opam-2.1.0-beta2-armhf-linux)     echo "fc4e6b753ce6368f75a0d3005f4b21ce9606599d21607a67015db55a38b6ef473b4205f5b128c5808189feed8ae58f93bd79348988be7c5007ae1b39307a5cd0";;
    opam-2.1.0-beta2-i686-linux)      echo "a376a6e0e1e2b08ea4d0a5c1c38487e67984bef2e89f978536dd08283f945f74dd31ee287bc68d91690603ba0fa657e91ff0d30bea217743f79ed99d2390eba5";;
    opam-2.1.0-beta2-x86_64-linux)    echo "12c5e2b0087ed389fa12fdb0e1f6f7dc0b3df3f95c59e8bc576279b7780921d47bbc4ebcba6caddde30f4fb1cc9e4a873cc8a6aef80fcc48a878aba69be7af44";;
    opam-2.1.0-beta2-x86_64-macos)    echo "4acc12672a2e3ad7e78540634edcae2e7e84860057b86a56b1cdf7eacf8d97957aaa864f571d6fb8f61ee8280f8a4ed73df7881d91a22c9d8c2d73e8a558f61d";;
    opam-2.1.0-beta2-x86_64-openbsd)  echo "84d7d409220c72e3ed7e6acdd7cce3b5a208f2966d232648a57a48641ab8ce4fa58e94e40b7176201455d82260e6c501a6ba4a30b1426a552f8d09cfd027ddde";;

    opam-2.1.0-beta3-arm64-linux)     echo "a58ba3ebb4431d3cabfe96b806c9897205153e8a546ebe74f0229982758d140b4fcbcea421db70589b1eb3080dc86534522a3cba0330ce82e0898a60048d51ba";;
    opam-2.1.0-beta3-armhf-linux)     echo "fc4e6b753ce6368f75a0d3005f4b21ce9606599d21607a67015db55a38b6ef473b4205f5b128c5808189feed8ae58f93bd79348988be7c5007ae1b39307a5cd0";;
    opam-2.1.0-beta3-i686-linux)      echo "a376a6e0e1e2b08ea4d0a5c1c38487e67984bef2e89f978536dd08283f945f74dd31ee287bc68d91690603ba0fa657e91ff0d30bea217743f79ed99d2390eba5";;
    opam-2.1.0-beta3-x86_64-linux)    echo "12c5e2b0087ed389fa12fdb0e1f6f7dc0b3df3f95c59e8bc576279b7780921d47bbc4ebcba6caddde30f4fb1cc9e4a873cc8a6aef80fcc48a878aba69be7af44";;
    opam-2.1.0-beta3-x86_64-macos)    echo "4acc12672a2e3ad7e78540634edcae2e7e84860057b86a56b1cdf7eacf8d97957aaa864f571d6fb8f61ee8280f8a4ed73df7881d91a22c9d8c2d73e8a558f61d";;
    opam-2.1.0-beta3-x86_64-openbsd)  echo "84d7d409220c72e3ed7e6acdd7cce3b5a208f2966d232648a57a48641ab8ce4fa58e94e40b7176201455d82260e6c501a6ba4a30b1426a552f8d09cfd027ddde";;

    opam-2.1.0-beta4-arm64-linux)     echo "f7278c732ec04703a729dccb4ca723d26ad537adf1d50faafa7e1ed5f063e3245e3d346e08029013d24e18978c8bc1f04b7b3f042d2c49db83ef3e18a40f109b";;
    opam-2.1.0-beta4-arm64-macos)     echo "1b69dc61f99c028797ec5b1908911ce9fa999878ecfb89e4675b9a12c41ceadec7cdb9ba912f9e6535d36101ae2f54fd10342016638ac10fcdb68631322e35c4";;
    opam-2.1.0-beta4-armhf-linux)     echo "b72f770e5d8033215fa53a9640b48dda5bed235daadd3174661879927cc4673db14666fd6ba71b51d47b3a278e45be57e99dd9104931ee5679980da186baf518";;
    opam-2.1.0-beta4-i686-linux)      echo "c68a850ef026dda9e14ad4c50a7a5a5730f6f83771c3acef99571c9884720292f0b547428ea7105d99003b6b56a975f8b3a925661992f5ce591deb69781d7a72";;
    opam-2.1.0-beta4-x86_64-linux)    echo "9a928c5c8e2e6c98f6da937839b1c9a063f7be7344d561f2e2a92423f0c8c227696145077278e8cc81a1fe6e9a3ca8b1af733001040d87e61bc28808bebdc08c";;
    opam-2.1.0-beta4-x86_64-macos)    echo "e3846b7eb9f5e1feea60a523670be4e47868714f7a337a5359f75ba984e0567780cf93b121b9789aba869cdec8954d0d71d8d12c9f92077ceccf6cb7922ec1f9";;
    opam-2.1.0-beta4-x86_64-openbsd)  echo "68f262b076bb3569e4ff680a307e02cf3f6ad07e22583466f396031612b94ec223fbc389d6883d9ea4eee4e0ee582f066c95189804136fd0f1da04dc28bbc3bc";;

    opam-2.1.0-rc-arm64-linux)     echo "cdd0c10a6f50a97c366017e6716e4e6f44fd93e2353fd5223df44ef4c99b4e214785d476d39abcf31109379ed57bb886b65e600a21ff2255849585cceb05b437";;
    opam-2.1.0-rc-arm64-macos)     echo "1972c7bd0cdba2d31d08abdc657684551eb35f0b608481ce6271ad792c88653739be2e4239ba62bc53dab8985c61a92fe2a5970bd663b3df3bb77faa92c68f1e";;
    opam-2.1.0-rc-armhf-linux)     echo "e827e97c58ff4c4d99b5bc5dd0a294ede99519f65d3414eda7e91b4b4191739686c62fcd6413fab1329bc65e49a60ae9038ae9e845541fddb8d1755cb43acc58";;
    opam-2.1.0-rc-i686-linux)      echo "29c766cd712cb500a40f7491154e1a1423c4a872b90fa53dbc35b25795e990b078d4951c30f7155d904c9218522cf29ea2c449a9cd3fd9e7d87b129ad1f2a335";;
    opam-2.1.0-rc-x86_64-linux)    echo "c77a693fdbd6d419420f9149d411516bf567cc049bc47710de143a6babdf1d17d5c5b0e149105bd970accd2ccc27fa2b0be6347ec685acd15e60ca4438f21b7d";;
    opam-2.1.0-rc-x86_64-macos)    echo "c1f2b312c8aea380f62a67fb26662c5a149ef0c22de9c80c27ca6fe50771a05f140d83d671a49486bfef0701dde4cf8a2a1fd750054f349d76fe6e1c2541a711";;
    opam-2.1.0-rc-x86_64-openbsd)  echo "ac0ff0beca6a15038b46a7fb35cb350e0993230189cd7940db5e91c6c3cb02fbe580ba4053259102abb88533bf681b25753bf3764b72b0b8e74d31f35990c6c0";;

    opam-2.1.0-rc2-arm64-linux)     echo "88540bd9a5164661528a7996da583b7336c1607698af80441932f939232a5423e6ac7bf970e06fb0fb09df23184b25ea77c1762125a32e1c2a5513456cbf74ed";;
    opam-2.1.0-rc2-arm64-macos)     echo "5fea7034c1178ad5c8725ac33882b499783073befb81ef6746912a93430592253cae955ca21e887b1e7a33f58a20a93e3e83ef8a9882ec02ad21f4053e2a879f";;
    opam-2.1.0-rc2-armhf-linux)     echo "18297ec40dffc1c84ad9d98fd2643bc60713482b854232d4eee955634431e1fe564d0cf1045ec17f148de35280c740849508b9464c138501cb3dd7acfeeca935";;
    opam-2.1.0-rc2-i686-linux)      echo "00547bbc9f3abd0bd19142168d4630f3869333fa3da0dbf9d1d07b6032204be6747f36eb397a2cc93bbcd04e0c103a5412f1b0f618a35ab57913332c385f622d";;
    opam-2.1.0-rc2-x86_64-linux)    echo "d07ea4a936a82d8fca17111f0c1a4d8915e90ca51f59245f9c60fcd082a54643160d2aeceae2e03113ef77568b867315307e88c06c58753ce7749be712b84bfd";;
    opam-2.1.0-rc2-x86_64-macos)    echo "48811556f65cd5afd3ad5b078c6f456df77e9e33cc2c14b289a2b17293a79f2417e99ae824cf704b953384e7a9ba47155839bd12b933efc9f0b7c6bc5033d470";;
    opam-2.1.0-rc2-x86_64-openbsd)  echo "2ae5e3d4820bad1cba322c43e12fd4d64d398f53887b520de734b49564071acd2224004151bbf4bdc6b572f037718b58d21c277157b1837cf71181fa7866bb08";;

    *) echo "no sha";;
  esac
}

usage() {
    echo "opam binary installer v.$VERSION"
    echo "Downloads and installs a pre-compiled binary of opam $VERSION to the system."
    echo "This can also be used to switch between opam versions"
    echo
    echo "Options:"
    echo "    --dev                  Install the latest alpha or beta instead: $DEV_VERSION"
    echo "    --no-backup            Don't attempt to backup the current opam root"
    echo "    --backup               Force the backup the current opam root (even if it"
    echo "                           is from the 2.0 branch already)"
    echo "    --fresh                Create the opam $VERSION root from scratch"
    echo "    --restore   VERSION    Restore a backed up opam binary and root"
    echo
    echo "The default is to backup if the current version of opam is 1.*, or when"
    echo "using '--fresh' or '--dev'"
}

RESTORE=
NOBACKUP=
FRESH=
DOWNLOAD_ONLY=

while [ $# -gt 0 ]; do
    case "$1" in
        --dev)
            VERSION=$DEV_VERSION
            if [ -z "$NOBACKUP" ]; then NOBACKUP=0; fi;;
        --restore)
            if [ $# -lt 2 ]; then echo "Option $1 requires an argument"; exit 2; fi
            shift;
            RESTORE=$1;;
        --no-backup)
            NOBACKUP=1;;
        --backup)
            NOBACKUP=0;;
        --fresh)
            FRESH=1;;
        --download-only)
            DOWNLOAD_ONLY=1;;
        --help|-h)
            usage; exit 0;;
        *)
            usage; exit 2;;
    esac
    shift
done


TMP=${TMPDIR:-/tmp}

ARCH=$(uname -m || echo unknown)
case "$ARCH" in
    x86|i?86) ARCH="i686";;
    x86_64|amd64) ARCH="x86_64";;
    ppc|powerpc|ppcle) ARCH="ppc";;
    aarch64_be|aarch64) ARCH="arm64";;
    armv5*|armv6*|earmv6*|armv7*|earmv7*|armv8b|armv8l) ARCH="armhf";;
    *) ARCH=$(echo "$ARCH" | awk '{print tolower($0)}')
esac

OS=$( (uname -s || echo unknown) | awk '{print tolower($0)}')

if [ "$OS" = "darwin" ] ; then
  OS=macos
fi

TAG=$(echo "$VERSION" | tr '~' '-')

OPAM_BIN_URL_BASE='https://github.com/ocaml/opam/releases/download/'
OPAM_BIN="opam-${TAG}-${ARCH}-${OS}"
OPAM_BIN_URL="${OPAM_BIN_URL_BASE}${TAG}/${OPAM_BIN}"

download() {
    if command -v wget >/dev/null; then wget -q -O "$@"
    else curl -s -L -o "$@"
    fi
}

check_sha512() {
    OPAM_BIN_LOC="$1"
    if command -v openssl > /dev/null; then
        sha512_devnull="cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e"
        sha512_check=`openssl sha512 2>&1 < /dev/null | cut -f 2 -d ' '`
        if [ "x$sha512_devnull" = "x$sha512_check" ]; then
            sha512=`openssl sha512 "$OPAM_BIN_LOC" 2> /dev/null | cut -f 2 -d ' '`
            check=`bin_sha512`
            test "x$sha512" = "x$check"
        else
            echo "openssl 512 option not handled, binary integrity check can't be performed."
            return 0
        fi
    else
        echo "openssl not found, binary integrity check can't be performed."
        return 0
    fi
}

download_and_check() {
    OPAM_BIN_LOC="$1"
    echo "## Downloading opam $VERSION for $OS on $ARCH..."

    if ! download "$OPAM_BIN_LOC" "$OPAM_BIN_URL"; then
        echo "There may not yet be a binary release for your architecture or OS, sorry."
        echo "See https://github.com/ocaml/opam/releases/tag/$TAG for pre-compiled binaries,"
        echo "or run 'make cold' from https://github.com/ocaml/opam/archive/$TAG.tar.gz"
        echo "to build from scratch"
        exit 10
    else
        if check_sha512 "$OPAM_BIN_LOC"; then
            echo "## Downloaded."
        else
            echo "Checksum mismatch, a problem occurred during download."
            exit 10
        fi
    fi
}

DOWNLOAD_ONLY=${DOWNLOAD_ONLY:-0}

if [ $DOWNLOAD_ONLY -eq 1 ]; then
    OPAM_BIN_LOC="$PWD/$OPAM_BIN"
    if [ -e "$OPAM_BIN_LOC" ]; then
        echo "Found opam binary in $OPAM_BIN_LOC ..."
        if check_sha512 "$OPAM_BIN_LOC" ; then
            echo "... with matching sha512"
            exit 0;
        else
            echo "... with mismatching sha512, download the good one."
        fi
    fi
    download_and_check "$OPAM_BIN_LOC"
    exit 0;
fi

EXISTING_OPAM=$(command -v opam || echo)
EXISTING_OPAMV=
if [ -n "$EXISTING_OPAM" ]; then
   EXISTING_OPAMV=$("$EXISTING_OPAM" --version || echo "unknown")
fi

FRESH=${FRESH:-0}

OPAMROOT=${OPAMROOT:-$HOME/.opam}

if [ ! -d "$OPAMROOT" ]; then FRESH=1; fi

if [ -z "$NOBACKUP" ] && [ ! "$FRESH" = 1 ] && [ -z "$RESTORE" ]; then
    case "$EXISTING_OPAMV" in
        2.*) NOBACKUP=1;;
        *) NOBACKUP=0;;
    esac
fi

xsudo() {
    local CMD=$1; shift
    local DST
    for DST in "$@"; do : ; done

    local DSTDIR=$(dirname "$DST")
    if [ ! -w "$DSTDIR" ]; then
        echo "Write access to $DSTDIR required, using 'sudo'."
        echo "Command: $CMD $@"
        if [ "$CMD" = "install" ]; then
            sudo "$CMD" -g 0 -o root "$@"
        else
            sudo "$CMD" "$@"
        fi
    else
        "$CMD" "$@"
    fi
}

if [ -n "$RESTORE" ]; then
    OPAM=$(command -v opam)
    OPAMV=$("$OPAM" --version)
    OPAM_BAK="$OPAM.$RESTORE"
    OPAMROOT_BAK="$OPAMROOT.$RESTORE"
    if [ ! -e "$OPAM_BAK" ] || [ ! -d "$OPAMROOT_BAK" ]; then
        echo "No backup of opam $RESTORE was found"
        exit 1
    fi
    if [ "$NOBACKUP" = 1 ]; then
        printf "## This will clear $OPAM and $OPAMROOT. Continue ? [Y/n] "
        read R
        case "$R" in
            ""|"y"|"Y"|"yes")
                xsudo rm -f "$OPAM"
                rm -rf "$OPAMROOT";;
            *) exit 1
        esac
    else
        xsudo mv "$OPAM" "$OPAM.$OPAMV"
        mv "$OPAMROOT" "$OPAMROOT.$OPAMV"
    fi
    xsudo mv "$OPAM_BAK" "$OPAM"
    mv "$OPAMROOT_BAK" "$OPAMROOT"
    printf "## Opam $RESTORE and its root were restored."
    if [ "$NOBACKUP" = 1 ]; then echo
    else echo " Opam $OPAMV was backed up."
    fi
    exit 0
fi

if [ -e "$TMP/$OPAM_BIN" ] && ! check_sha512 "$TMP/$OPAM_BIN" || [ ! -e "$TMP/$OPAM_BIN" ]; then
    download_and_check "$TMP/$OPAM_BIN"
else
    echo "## Using already downloaded \"$TMP/$OPAM_BIN\""
fi

if [ -n "$EXISTING_OPAM" ]; then
    DEFAULT_BINDIR=$(dirname "$EXISTING_OPAM")
fi

while true; do
    printf "## Where should it be installed ? [$DEFAULT_BINDIR] "
    read BINDIR
    if [ -z "$BINDIR" ]; then BINDIR="$DEFAULT_BINDIR"; fi

    if [ -d "$BINDIR" ]; then break
    else
        printf "## $BINDIR does not exist. Create ? [Y/n] "
        read R
        case "$R" in
            ""|"y"|"Y"|"yes")
            xsudo mkdir -p $BINDIR
            break;;
        esac
    fi
done

if [ -e "$EXISTING_OPAM" ]; then
    if [ "$NOBACKUP" = 1 ]; then
        xsudo rm -f "$EXISTING_OPAM"
    else
        xsudo mv "$EXISTING_OPAM" "$EXISTING_OPAM.$EXISTING_OPAMV"
        echo "## $EXISTING_OPAM backed up as $(basename $EXISTING_OPAM).$EXISTING_OPAMV"
    fi
fi

if [ -d "$OPAMROOT" ]; then
    if [ "$FRESH" = 1 ]; then
        if [ "$NOBACKUP" = 1 ]; then
            printf "## This will clear $OPAMROOT. Continue ? [Y/n] "
            read R
            case "$R" in
                ""|"y"|"Y"|"yes")
                    rm -rf "$OPAMROOT";;
                *) exit 1
            esac
        else
            mv "$OPAMROOT" "$OPAMROOT.$EXISTING_OPAMV"
            echo "## $OPAMROOT backed up as $(basename $OPAMROOT).$EXISTING_OPAMV"
        fi
        echo "## opam $VERSION installed. Please run 'opam init' to get started"
    elif [ ! "$NOBACKUP" = 1 ]; then
        echo "## Backing up $OPAMROOT to $(basename $OPAMROOT).$EXISTING_OPAMV (this may take a while)"
        if [ -e "$OPAMROOT.$EXISTING_OPAMV" ]; then
            echo "ERROR: there is already a backup at $OPAMROOT.$EXISTING_OPAMV"
            echo "Please move it away or run with --no-backup"
        fi
        FREE=$(df -k "$OPAMROOT" | awk 'NR>1 {print $4}')
        NEEDED=$(du -sk "$OPAMROOT" | awk '{print $1}')
        if ! [ $NEEDED -lt $FREE ]; then
            echo "Error: not enough free space to backup. You can retry with --no-backup,"
            echo "--fresh, or remove '$OPAMROOT'"
            exit 1
        fi
        cp -a "$OPAMROOT" "$OPAMROOT.$EXISTING_OPAMV"
        echo "## $OPAMROOT backed up as $(basename $OPAMROOT).$EXISTING_OPAMV"
    fi
    rm -f "$OPAMROOT"/repo/*/*.tar.gz*
fi

xsudo install -m 755 "$TMP/$OPAM_BIN" "$BINDIR/opam"
echo "## opam $VERSION installed to $BINDIR"

if [ ! "$FRESH" = 1 ]; then
    echo "## Converting the opam root format & updating"
    "$BINDIR/opam" init --reinit -ni
fi

WHICH=$(command -v opam || echo notfound)

case "$WHICH" in
    "$BINDIR/opam") ;;
    notfound) echo "## Remember to add $BINDIR to your PATH";;
    *)
        echo "## WARNING: 'opam' command found in PATH does not match the installed one:"
        echo "   - Installed: '$BINDIR/opam'"
        echo "   - Found:     '$WHICH'"
        echo "Make sure to remove the second or fix your PATH to use the new opam"
        echo
esac

if [ ! "$NOBACKUP" = 1 ]; then
    echo "## Run this script again with '--restore $EXISTING_OPAMV' to revert."
fi

rm -f $TMP/$OPAM_BIN
