// Upgrade NOTE: replaced '_World2Object' with 'unity_WorldToObject'

Shader "Custom/DiffuseSimpleWGlobals" {
	SubShader {
		Tags { "LightMode" = "ForwardBase" }
		Pass {
			CGPROGRAM
			#include "UnityCG.cginc"
			#pragma target 2.0
			#pragma vertex vert
			#pragma fragment frag

			float4 _LightColor0;

			struct vsIn {
				float4 position : POSITION;
				float3 normal : NORMAL;
			};

			struct vsOut {
				float4 position : SV_POSITION;
				float3 normal : NORMAL;
			};

			vsOut vert(vsIn v){
				vsOut o;
				o.position = UnityObjectToClipPos(v.position);
				o.normal = normalize(mul(float4(v.normal, 0.0), unity_WorldToObject));
				return o;
			}

			float4 frag(vsOut psIn) : SV_TARGET {
				float4 ambientLight = UNITY_LIGHTMODEL_AMBIENT;

				float4 LightDirection = normalize(_WorldSpaceLightPos0);
				float4 diffuseTerm = saturate(dot(LightDirection, psIn.normal));
				float4 diffuseLight = diffuseTerm * _LightColor0;

				return ambientLight + diffuseLight;
			}
			ENDCG
		}
	}
}
