Shader "Custom/DebugNormals" {
	Properties {
		_Amnt ("Amount", float) = 1.0
	}

	SubShader {
		Pass {
			CGPROGRAM
			#include "UnityCG.cginc"
			#pragma target 2.0
			#pragma vertex vert
			#pragma fragment frag

			float4 _LightColor0;
			float _Amnt;

			struct vsIn {
				float4 position : POSITION;
				float3 normal : NORMAL;
			};

			struct vsOut {
				float4 position : SV_POSITION;
				fixed4 color : COLOR;
			};

			vsOut vert(vsIn v){
				vsOut o;
				o.position = UnityObjectToClipPos(v.position);
				o.color.xyz = v.normal * 0.5 + 0.5;
				o.color.w = 1.0;
				return o;
			}

			float4 frag(vsOut psIn) : SV_TARGET {
				return psIn.color;
			}
			ENDCG
		}
	}
}
